{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Emanote.Model.Note.Filter (
  PandocFilterDeclarations (..),
  applyParsePandocFilters,
  applyRenderHtmlPandocFilters,
  lookupPandocFilterDeclarations,
  filterDeclarationShapeErrors,
) where

import Control.Exception qualified as CE
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Writer.Strict (MonadWriter (tell))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as Aeson
import Data.Char (isAlpha, isAlphaNum)
import Data.Default (def)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Emanote.Model.SData qualified as SData
import Emanote.Prelude (log, logE)
import Relude
import System.Directory (doesFileExist, doesPathExist, getTemporaryDirectory, removeFile)
import System.FilePath (takeExtension, takeFileName, (</>))
import System.IO (hClose, openTempFile)
import System.IO.Error (catchIOError)
import Text.Pandoc (runIO)
import Text.Pandoc.Definition (Meta (..), MetaValue (..), Pandoc (..))
import Text.Pandoc.Filter qualified as PF
import Text.Pandoc.Scripting (ScriptingEngine)
import UnliftIO.Exception (handle)

-- | Lua filters declared by a note, split by when Emanote applies them.
data PandocFilterDeclarations = PandocFilterDeclarations
  { pfdParseFilters :: [FilePath]
  , pfdRenderHtmlFilters :: [FilePath]
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Aeson.ToJSON)

instance Semigroup PandocFilterDeclarations where
  PandocFilterDeclarations a b <> PandocFilterDeclarations a' b' =
    PandocFilterDeclarations (a <> a') (b <> b')

instance Monoid PandocFilterDeclarations where
  mempty = PandocFilterDeclarations mempty mempty

lookupPandocFilterDeclarations :: Aeson.Value -> PandocFilterDeclarations
lookupPandocFilterDeclarations frontmatter =
  PandocFilterDeclarations
    { pfdParseFilters =
        SData.lookupAeson @[FilePath] mempty ("pandoc" :| ["filters", "parse"]) frontmatter
    , pfdRenderHtmlFilters =
        SData.lookupAeson @[FilePath] mempty ("pandoc" :| ["filters", "render", "html"]) frontmatter
    }

{- | Diagnose frontmatter shape errors at the @pandoc.filters.{parse,render.html}@
keys. 'lookupPandocFilterDeclarations' silently degrades to @mempty@ on a
type mismatch (e.g. a YAML string where a list of paths was expected),
which would silently disable the filter. This walks the same paths and
reports any value that isn't a list of strings, so callers can 'tell' a
diagnostic instead.
-}
filterDeclarationShapeErrors :: Aeson.Value -> [Text]
filterDeclarationShapeErrors frontmatter =
  mapMaybe (uncurry checkPath) keysToCheck
  where
    keysToCheck :: [(Text, [Text])]
    keysToCheck =
      [ ("pandoc.filters.parse", ["pandoc", "filters", "parse"])
      , ("pandoc.filters.render.html", ["pandoc", "filters", "render", "html"])
      ]
    checkPath :: Text -> [Text] -> Maybe Text
    checkPath label path =
      case lookupRaw path frontmatter of
        Nothing -> Nothing
        Just Aeson.Null -> Nothing
        Just (Aeson.Array xs) | all isString xs -> Nothing
        Just other ->
          Just
            $ "Bad shape for "
            <> label
            <> ": expected a list of file paths, got "
            <> describe other
            <> ". The declaration was ignored."
    isString = \case
      Aeson.String _ -> True
      _ -> False
    lookupRaw :: [Text] -> Aeson.Value -> Maybe Aeson.Value
    lookupRaw [] v = Just v
    lookupRaw (k : ks) (Aeson.Object o) =
      KeyMap.lookup (fromString $ toString k) o >>= lookupRaw ks
    lookupRaw _ _ = Nothing
    describe :: Aeson.Value -> Text
    describe = \case
      Aeson.String _ -> "a string"
      Aeson.Number _ -> "a number"
      Aeson.Bool _ -> "a boolean"
      Aeson.Object _ -> "an object"
      Aeson.Array _ -> "a list with non-string entries"
      Aeson.Null -> "null"

{- | Resolve and apply a note's declared parse-time Lua filters.

Returns the requested filter paths unchanged alongside the filtered document so
the source-dependency index records the declaration form, not the resolved
absolute path. Parse-time-specific validation (rejecting filters that touch
IO-capable APIs) lives here, not on a generic application record — render-time
filters are allowed to use IO.
-}
applyParsePandocFilters ::
  (MonadIO m, MonadLogger m, MonadWriter [Text] m) =>
  ScriptingEngine ->
  [FilePath] ->
  PandocFilterDeclarations ->
  Pandoc ->
  m (Pandoc, [FilePath])
applyParsePandocFilters scriptingEngine pluginBaseDir declarations doc = do
  resolvedFilters <- resolveLuaFilters pluginBaseDir (pfdParseFilters declarations)
  ioCleanFilters <- filterM rejectParseTimeIO resolvedFilters
  -- Acquire all guarded-temp files atomically: a mid-list write failure
  -- triggers cleanup of the partial successes before re-raising, instead
  -- of leaking them into $TMPDIR. Pandoc errors during applyPandocFilters
  -- are caught internally by runIOCatchingErrors, so the cleanup call
  -- below runs in normal flow; the only remaining leak window is an
  -- async exception between acquire and the post-apply cleanup, accepted
  -- rather than threading MonadUnliftIO/MonadMask through the WriterT
  -- stack here.
  guardedPaths <- liftIO $ acquireGuardedFilters ioCleanFilters
  filteredDoc <- applyPandocFilters scriptingEngine "markdown" (PF.LuaFilter <$> guardedPaths) doc
  liftIO $ cleanupGuardedParseFilters guardedPaths
  pure (filteredDoc, pfdParseFilters declarations)
  where
    rejectParseTimeIO rf = do
      uses <- liftIO $ parseTimeFilterIOUsesIn rf
      if null uses
        then pure True
        else tell [parseTimeFilterIOErrorMsg rf uses] >> pure False

acquireGuardedFilters :: [ResolvedPandocFilter] -> IO [FilePath]
acquireGuardedFilters = go []
  where
    go acc [] = pure (reverse acc)
    go acc (f : rest) = do
      p <- writeGuardedParseFilter f `CE.onException` cleanupGuardedParseFilters acc
      go (p : acc) rest

applyRenderHtmlPandocFilters ::
  (MonadIO m, MonadLogger m, MonadWriter [Text] m) =>
  ScriptingEngine ->
  [FilePath] ->
  PandocFilterDeclarations ->
  Aeson.Value ->
  Pandoc ->
  m Pandoc
applyRenderHtmlPandocFilters scriptingEngine pluginBaseDir declarations meta doc = do
  resolvedFilters <- resolveLuaFilters pluginBaseDir (pfdRenderHtmlFilters declarations)
  applyPandocFilters scriptingEngine "html" (PF.LuaFilter . rpfResolvedPath <$> resolvedFilters) (withPandocMeta meta doc)

data ResolvedPandocFilter = ResolvedPandocFilter
  { rpfRequestedPath :: FilePath
  , rpfResolvedPath :: FilePath
  }
  deriving stock (Eq, Ord, Show)

resolveLuaFilters ::
  (MonadIO m, MonadWriter [Text] m) =>
  [FilePath] ->
  [FilePath] ->
  m [ResolvedPandocFilter]
resolveLuaFilters pluginBaseDir requestedFilters = do
  resolvedFilters <- resolvePandocFilterPaths pluginBaseDir requestedFilters
  fmap catMaybes $ forM resolvedFilters $ \(requestedPath, resolvedPath) ->
    checkLuaFilter requestedPath resolvedPath >>= \case
      Left err -> tell [err] >> pure Nothing
      Right resolvedFilter -> pure $ Just resolvedFilter

resolvePandocFilterPaths ::
  (MonadIO m, MonadWriter [Text] m) =>
  [FilePath] ->
  [FilePath] ->
  m [(FilePath, FilePath)]
resolvePandocFilterPaths pluginBaseDir requestedFilters =
  fmap catMaybes $ forM requestedFilters $ \p -> do
    res :: [FilePath] <- flip mapMaybeM pluginBaseDir $ \baseDir -> do
      liftIO (doesPathExist $ baseDir </> p) >>= \case
        False -> do
          pure Nothing
        True ->
          pure $ Just $ baseDir </> p
    case res of
      [] -> do
        tell [toText $ "Pandoc filter " <> p <> " not found in any of: " <> show pluginBaseDir]
        pure Nothing
      (x : _) -> pure $ Just (p, x)

applyPandocFilters :: (MonadIO m, MonadLogger m, MonadWriter [Text] m) => ScriptingEngine -> String -> [PF.Filter] -> Pandoc -> m Pandoc
applyPandocFilters scriptingEngine format paths doc = do
  case paths of
    [] ->
      pure doc
    filters ->
      applyPandocLuaFilters scriptingEngine format filters doc >>= \case
        Left err -> tell [err] >> pure doc
        Right x -> pure x

checkLuaFilter :: (MonadIO m) => FilePath -> FilePath -> m (Either Text ResolvedPandocFilter)
checkLuaFilter requestedPath resolvedPath = do
  if takeExtension requestedPath == ".lua"
    then do
      liftIO (doesFileExist resolvedPath) >>= \case
        True -> pure $ Right $ ResolvedPandocFilter requestedPath resolvedPath
        False -> pure $ Left $ toText $ "Lua filter missing: " <> requestedPath
    else pure $ Left $ "Unsupported filter: " <> toText requestedPath

-- Pandoc's Lua filter runner takes a file path, not an in-memory script.
-- Parse-time filters therefore run through guarded temporary copies so the
-- no-IO prelude executes in the same Lua chunk as the user's filter.
writeGuardedParseFilter :: ResolvedPandocFilter -> IO FilePath
writeGuardedParseFilter ResolvedPandocFilter {..} = do
  tmpDir <- getTemporaryDirectory
  (guardedPath, h) <- openTempFile tmpDir $ "emanote-parse-" <> takeFileName rpfResolvedPath
  source <- decodeUtf8 <$> readFileBS rpfResolvedPath
  TIO.hPutStr h $ parseTimeNoIOPrelude <> "\n" <> source
  hClose h
  pure guardedPath

cleanupGuardedParseFilters :: [FilePath] -> IO ()
cleanupGuardedParseFilters =
  traverse_ $ \path ->
    -- Best-effort cleanup: if the temp file is already gone, there is no
    -- semantic state to recover and the OS temp cleaner can handle stragglers.
    removeFile path `catchIOError` const pass

{- | Lua chunk prepended to every parse-time filter to enforce the no-IO
policy at runtime. Generated from the same name lists the static scanner
uses ('standaloneIOApis', 'pandocIOMembers', 'pandocUtilsIOMembers') so the
two layers of defence can't drift.

Emitted on a single physical line so a runtime/syntax error in the user's
filter still reports nearly the right line number (chunk line N + 1 = user
file line N).
-}
parseTimeNoIOPrelude :: Text
parseTimeNoIOPrelude =
  T.intercalate " "
    $ [ "local function emanote_no_parse_io(name) return function() error('Parse-time Lua filters cannot use IO: ' .. name, 2) end end;"
      , "local emanote_outer_env = _ENV;"
      , "local emanote_pandoc = {};"
      , "if pandoc then for k, v in pairs(pandoc) do emanote_pandoc[k] = v end end;"
      , "if type(emanote_pandoc.utils) == 'table' then local emanote_utils = {} for k, v in pairs(emanote_pandoc.utils) do emanote_utils[k] = v end emanote_pandoc.utils = emanote_utils end;"
      ]
    <> map (banLuaMember "emanote_pandoc" "pandoc") pandocIOMembers
    <> [ "if emanote_pandoc.utils then "
          <> T.intercalate " " (map (banLuaMember "emanote_pandoc.utils" "pandoc.utils") pandocUtilsIOMembers)
          <> " end;"
       ]
    <> [ "local emanote_env = setmetatable({"
          <> T.intercalate ", " (map standaloneEntry standaloneIOApis <> ["pandoc = emanote_pandoc"])
          <> "}, { __index = emanote_outer_env, __newindex = emanote_outer_env, __metatable = false });"
       , "_ENV = emanote_env"
       ]
  where
    banLuaMember accessExpr humanName api =
      let q = ioApiName api
       in accessExpr <> "." <> q <> " = " <> banExpression (humanName <> "." <> q) (ioApiKind api) <> ";"
    standaloneEntry api =
      ioApiName api <> " = " <> banExpression (ioApiName api) (ioApiKind api)
    banExpression diagName = \case
      IOCallable -> "emanote_no_parse_io('" <> diagName <> "')"
      IOTable -> "false"

applyPandocLuaFilters :: (MonadIO m, MonadLogger m) => ScriptingEngine -> String -> [PF.Filter] -> Pandoc -> m (Either Text Pandoc)
applyPandocLuaFilters scriptingEngine format filters x = do
  log $ "Applying pandoc filters (" <> toText format <> "): " <> show filters
  liftIO (runIOCatchingErrors $ PF.applyFilters scriptingEngine def filters [format] x) >>= \case
    Left err -> do
      logE $ "Error applying pandoc filters: " <> show err
      pure $ Left (show err)
    Right x' -> pure $ Right x'
  where
    -- `runIO` can throw `PandocError`. Fix this nonsense behaviour, by catching
    -- it and returning a `Left`.
    runIOCatchingErrors =
      handle (pure . Left) . runIO

parseTimeFilterIOUsesIn :: ResolvedPandocFilter -> IO [Text]
parseTimeFilterIOUsesIn ResolvedPandocFilter {..} =
  parseTimeFilterIOUses . decodeUtf8 <$> readFileBS rpfResolvedPath

parseTimeFilterIOErrorMsg :: ResolvedPandocFilter -> [Text] -> Text
parseTimeFilterIOErrorMsg ResolvedPandocFilter {..} uses =
  "Parse-time Lua filters cannot use IO-capable APIs: "
    <> toText rpfRequestedPath
    <> " references "
    <> T.intercalate ", " uses
    <> ". Move the filter to pandoc.filters.render.html if it needs IO."

parseTimeFilterIOUses :: Text -> [Text]
parseTimeFilterIOUses =
  ordNub . bannedUses . luaTokens . toString

data LuaToken
  = LuaName Text
  | LuaString Text
  | LuaDot
  | LuaLBracket
  | LuaRBracket
  deriving stock (Eq, Show)

bannedUses :: [LuaToken] -> [Text]
bannedUses tokens =
  standaloneUses <> pandocUses tokens
  where
    standaloneUses =
      [ name
      | LuaName name <- tokens
      , name `elem` standaloneIOGlobals
      ]
    pandocUses = \case
      LuaName "pandoc" : rest
        | Just (member, rest') <- luaMemberAccess rest
        , member `elem` ioApiNames pandocIOMembers ->
            ("pandoc." <> member) : pandocUses rest'
        | Just ("utils", rest') <- luaMemberAccess rest
        , Just (member, rest'') <- luaMemberAccess rest'
        , member `elem` ioApiNames pandocUtilsIOMembers ->
            ("pandoc.utils." <> member) : pandocUses rest''
      _ : rest ->
        pandocUses rest
      [] ->
        []

{- | A Lua name the parse-time filter must not touch. The 'IOKind'
discriminates how the runtime sandbox neuters the name: callables get an
error-thunk so the call site fails loudly, table-typed names get @false@
so the user filter chokes on the first index access.
-}
data IOApi = IOApi
  { ioApiName :: Text
  , ioApiKind :: IOKind
  }
  deriving stock (Eq, Show)

data IOKind = IOCallable | IOTable
  deriving stock (Eq, Show)

ioApiNames :: [IOApi] -> [Text]
ioApiNames = fmap ioApiName

standaloneIOApis :: [IOApi]
standaloneIOApis =
  [ IOApi "io" IOTable
  , IOApi "os" IOTable
  , IOApi "package" IOTable
  , IOApi "debug" IOTable
  , IOApi "_G" IOTable
  , -- _ENV is shadowed via setmetatable, not via the entry table itself,
    -- so it appears only in the static scanner's banned-name list below.
    IOApi "require" IOCallable
  , IOApi "dofile" IOCallable
  , IOApi "loadfile" IOCallable
  , IOApi "load" IOCallable
  , IOApi "loadstring" IOCallable
  , IOApi "print" IOCallable
  , IOApi "warn" IOCallable
  ]

{- | Names the static scanner flags as IO references, including @_ENV@
which the runtime sandbox handles via metatable rather than table entry.
-}
standaloneIOGlobals :: [Text]
standaloneIOGlobals = "_ENV" : ioApiNames standaloneIOApis

pandocIOMembers :: [IOApi]
pandocIOMembers =
  [ IOApi "pipe" IOCallable
  , IOApi "system" IOTable
  , IOApi "mediabag" IOTable
  , IOApi "image" IOTable
  , IOApi "cli" IOTable
  , IOApi "template" IOTable
  , IOApi "zip" IOTable
  , IOApi "log" IOTable
  ]

pandocUtilsIOMembers :: [IOApi]
pandocUtilsIOMembers =
  [ IOApi "run_json_filter" IOCallable
  , IOApi "run_lua_filter" IOCallable
  ]

luaMemberAccess :: [LuaToken] -> Maybe (Text, [LuaToken])
luaMemberAccess = \case
  LuaDot : LuaName member : rest ->
    Just (member, rest)
  LuaLBracket : LuaString member : LuaRBracket : rest ->
    Just (member, rest)
  _ ->
    Nothing

luaTokens :: String -> [LuaToken]
luaTokens = \case
  '-' : '-' : '[' : rest -> case openLongBracket rest of
    Just (level, body) -> luaTokens $ dropLongString level body
    Nothing -> luaTokens $ drop 1 $ dropWhile (/= '\n') rest
  '-' : '-' : rest ->
    luaTokens $ drop 1 $ dropWhile (/= '\n') rest
  '[' : rest -> case openLongBracket rest of
    Just (level, body) -> luaTokens $ dropLongString level body
    Nothing -> LuaLBracket : luaTokens rest
  ']' : rest ->
    LuaRBracket : luaTokens rest
  '"' : rest ->
    let (s, rest') = takeQuoted '"' rest
     in LuaString (toText s) : luaTokens rest'
  '\'' : rest ->
    let (s, rest') = takeQuoted '\'' rest
     in LuaString (toText s) : luaTokens rest'
  '.' : rest ->
    LuaDot : luaTokens rest
  c : rest
    | isLuaIdentStart c ->
        let (nameRest, rest') = span isLuaIdentContinue rest
         in LuaName (toText $ c : nameRest) : luaTokens rest'
    | otherwise ->
        luaTokens rest
  [] ->
    []
  where
    isLuaIdentStart c =
      isAlpha c || c == '_'
    isLuaIdentContinue c =
      isAlphaNum c || c == '_'
    -- After consuming a leading '[', try to parse the rest of a Lua long-
    -- bracket opener: any number of '=' signs followed by another '['.
    -- Returns (level, body) on success, where level is the equals count
    -- and body is the input past the entire opener.
    openLongBracket :: String -> Maybe (Int, String)
    openLongBracket s =
      let (eqs, after) = span (== '=') s
       in case after of
            '[' : body -> Just (length eqs, body)
            _ -> Nothing
    -- Skip past the matching long-bracket close: ']' + N '=' + ']'.
    dropLongString :: Int -> String -> String
    dropLongString level = go
      where
        go = \case
          ']' : rest ->
            let (eqs, after) = span (== '=') rest
             in case after of
                  ']' : tailRest | length eqs == level -> tailRest
                  _ -> go rest
          _ : rest -> go rest
          [] -> []
    takeQuoted quote =
      go []
      where
        go acc = \case
          '\\' : c : rest ->
            go (c : acc) rest
          c : rest
            | c == quote -> (reverse acc, rest)
            | otherwise -> go (c : acc) rest
          [] ->
            (reverse acc, [])
withPandocMeta :: Aeson.Value -> Pandoc -> Pandoc
withPandocMeta meta (Pandoc _ blocks) =
  Pandoc (aesonToPandocMeta meta) blocks

aesonToPandocMeta :: Aeson.Value -> Meta
aesonToPandocMeta = \case
  Aeson.Object o -> Meta $ Map.mapMaybe aesonToPandocMetaValue $ KeyMap.toMapText o
  _ -> mempty

aesonToPandocMetaValue :: Aeson.Value -> Maybe MetaValue
aesonToPandocMetaValue = \case
  Aeson.Object o ->
    Just . MetaMap . Map.mapMaybe aesonToPandocMetaValue $ KeyMap.toMapText o
  Aeson.Array xs ->
    Just . MetaList $ mapMaybe aesonToPandocMetaValue (toList xs)
  Aeson.String s ->
    Just $ MetaString s
  Aeson.Number n ->
    Just . MetaString $ show n
  Aeson.Bool b ->
    Just $ MetaBool b
  Aeson.Null ->
    Nothing
