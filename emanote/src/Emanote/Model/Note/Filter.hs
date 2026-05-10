{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Emanote.Model.Note.Filter (
  PandocFilterDeclarations (..),
  applyParsePandocFilters,
  applyRenderHtmlPandocFilters,
  lookupPandocFilterDeclarations,
) where

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

lookupPandocFilterDeclarations :: Aeson.Value -> PandocFilterDeclarations
lookupPandocFilterDeclarations frontmatter =
  PandocFilterDeclarations
    { pfdParseFilters =
        SData.lookupAeson @[FilePath] mempty ("pandoc" :| ["filters", "parse"]) frontmatter
    , pfdRenderHtmlFilters =
        SData.lookupAeson @[FilePath] mempty ("pandoc" :| ["filters", "render", "html"]) frontmatter
    }

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
  guardedPaths <- liftIO $ traverse writeGuardedParseFilter ioCleanFilters
  filteredDoc <- applyPandocFilters scriptingEngine "markdown" (PF.LuaFilter <$> guardedPaths) doc
  liftIO $ cleanupGuardedParseFilters guardedPaths
  pure (filteredDoc, pfdParseFilters declarations)
  where
    rejectParseTimeIO rf = do
      uses <- liftIO $ parseTimeFilterIOUsesIn rf
      if null uses
        then pure True
        else tell [parseTimeFilterIOErrorMsg rf uses] >> pure False

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

parseTimeNoIOPrelude :: Text
parseTimeNoIOPrelude =
  unlines
    [ "local function emanote_no_parse_io(name)"
    , "  return function() error('Parse-time Lua filters cannot use IO: ' .. name, 2) end"
    , "end"
    , "local emanote_outer_env = _ENV"
    , "local emanote_pandoc = {}"
    , "if pandoc then"
    , "  for k, v in pairs(pandoc) do emanote_pandoc[k] = v end"
    , "end"
    , "if type(emanote_pandoc.utils) == 'table' then"
    , "  local emanote_utils = {}"
    , "  for k, v in pairs(emanote_pandoc.utils) do emanote_utils[k] = v end"
    , "  emanote_pandoc.utils = emanote_utils"
    , "end"
    , "emanote_pandoc.pipe = emanote_no_parse_io('pandoc.pipe')"
    , "emanote_pandoc.system = false"
    , "emanote_pandoc.mediabag = false"
    , "emanote_pandoc.image = false"
    , "emanote_pandoc.cli = false"
    , "emanote_pandoc.template = false"
    , "emanote_pandoc.zip = false"
    , "emanote_pandoc.log = false"
    , "if emanote_pandoc.utils then"
    , "  emanote_pandoc.utils.run_json_filter = emanote_no_parse_io('pandoc.utils.run_json_filter')"
    , "  emanote_pandoc.utils.run_lua_filter = emanote_no_parse_io('pandoc.utils.run_lua_filter')"
    , "end"
    , "local emanote_env = setmetatable({"
    , "  io = false,"
    , "  os = false,"
    , "  package = false,"
    , "  debug = false,"
    , "  _G = false,"
    , "  require = emanote_no_parse_io('require'),"
    , "  dofile = emanote_no_parse_io('dofile'),"
    , "  loadfile = emanote_no_parse_io('loadfile'),"
    , "  load = emanote_no_parse_io('load'),"
    , "  loadstring = emanote_no_parse_io('loadstring'),"
    , "  print = emanote_no_parse_io('print'),"
    , "  warn = emanote_no_parse_io('warn'),"
    , "  pandoc = emanote_pandoc,"
    , "}, { __index = emanote_outer_env, __newindex = emanote_outer_env, __metatable = false })"
    , "_ENV = emanote_env"
    ]

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
        , member `elem` pandocIOMembers ->
            ("pandoc." <> member) : pandocUses rest'
        | Just ("utils", rest') <- luaMemberAccess rest
        , Just (member, rest'') <- luaMemberAccess rest'
        , member `elem` pandocUtilsIOMembers ->
            ("pandoc.utils." <> member) : pandocUses rest''
      _ : rest ->
        pandocUses rest
      [] ->
        []

standaloneIOGlobals :: [Text]
standaloneIOGlobals =
  ["io", "os", "package", "require", "dofile", "loadfile", "load", "loadstring", "debug", "print", "warn", "_G", "_ENV"]

pandocIOMembers :: [Text]
pandocIOMembers =
  ["pipe", "system", "mediabag", "image", "cli", "template", "zip", "log"]

pandocUtilsIOMembers :: [Text]
pandocUtilsIOMembers =
  ["run_json_filter", "run_lua_filter"]

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
  '-' : '-' : '[' : '[' : rest ->
    luaTokens $ dropLuaLongString rest
  '-' : '-' : rest ->
    luaTokens $ drop 1 $ dropWhile (/= '\n') rest
  '[' : '[' : rest ->
    luaTokens $ dropLuaLongString rest
  '[' : rest ->
    LuaLBracket : luaTokens rest
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
    dropLuaLongString = \case
      ']' : ']' : rest ->
        rest
      _ : rest ->
        dropLuaLongString rest
      [] ->
        []
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
