-- | Patch model state depending on file change event.
module Emanote.Source.Patch (
  patchModel,
  filePatterns,
  ignorePatterns,
) where

import Control.Monad.Logger (LoggingT (runLoggingT), MonadLogger, MonadLoggerIO (askLoggerIO))
import Data.ByteString qualified as BS
import Data.List qualified as List
import Data.List.NonEmpty qualified as NEL
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Emanote.Model qualified as M
import Emanote.Model.Note qualified as N
import Emanote.Model.SData qualified as SD
import Emanote.Model.SourceDependencies qualified as SDeps
import Emanote.Model.StaticFile (readStaticFileInfo)
import Emanote.Model.Stork.Index qualified as Stork
import Emanote.Model.Type (ModelEma, modelSourceDependencies)
import Emanote.Prelude (
  chainM,
  log,
  logD,
  logE,
 )
import Emanote.Route qualified as R
import Emanote.Source.Loc (Loc, locMountPoint, locPath, locResolve)
import Emanote.Source.Pattern (filePatterns, ignorePatterns)
import Heist.Extra.TemplateState qualified as T
import Optics.Operators ((%~), (^.))
import Relude
import Relude.Extra (traverseToSnd)
import System.UnionMount qualified as UM
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Directory (doesDirectoryExist)

{- | Map a filesystem change to the corresponding model change.

The streaming handler in 'Source.Dynamic' hands every per-file step
the running model; we forward it uniformly so 'patchModel' has one
shape across all file types. Note parsing reads the model's Pandoc
scripting engine, and the 'R.LuaFilter' branch also walks the
reverse-dependency index in @Emanote.Model.SourceDependencies@. The
returned transformer is applied to that same model by the caller.
-}
patchModel ::
  (MonadIO m, MonadLogger m, MonadLoggerIO m) =>
  Set Loc ->
  (N.Note -> N.Note) ->
  Stork.IndexVar ->
  ModelEma ->
  R.FileType R.SourceExt ->
  FilePath ->
  UM.FileAction (NonEmpty (Loc, FilePath)) ->
  m (ModelEma -> ModelEma)
patchModel layers noteF storkIndexTVar model fpType fp action = do
  logger <- askLoggerIO
  now <- liftIO getCurrentTime
  -- Prefix all patch logging with timestamp.
  let newLogger loc src lvl s =
        logger loc src lvl $ fromString (formatTime defaultTimeLocale "[%H:%M:%S] " now) <> s
  runLoggingT (patchModel' layers noteF storkIndexTVar model fpType fp action) newLogger

patchModel' ::
  (MonadIO m, MonadLogger m) =>
  Set Loc ->
  (N.Note -> N.Note) ->
  Stork.IndexVar ->
  ModelEma ->
  R.FileType R.SourceExt ->
  FilePath ->
  UM.FileAction (NonEmpty (Loc, FilePath)) ->
  m (ModelEma -> ModelEma)
patchModel' layers noteF storkIndexTVar model fpType fp action = do
  sourcePatch <- case fpType of
    R.LMLType lmlType -> do
      case R.mkLMLRouteFromKnownFilePath lmlType fp of
        Nothing ->
          pure id -- Impossible
        Just r -> do
          -- Stork doesn't support incremental building of index, so we must
          -- clear it to pave way for a rebuild later when requested.
          --
          -- From https://github.com/jameslittle230/stork/discussions/112#discussioncomment-252861
          --
          -- > Stork also doesn't support incremental index updates today --
          -- you'd have to re-index everything when users added a new document,
          -- which might be prohibitively long.
          Stork.clearStorkIndex storkIndexTVar

          case action of
            UM.Refresh refreshAction overlays ->
              parseAndInsert layers noteF model refreshAction r (head overlays)
            UM.Delete -> do
              log $ "Removing note: " <> toText fp
              pure $ M.modelDeleteNote r
    R.LuaFilter -> do
      -- An edit (or deletion) of a Pandoc Lua filter file invalidates
      -- every note that declared it. The dep index
      -- carries each dependent's @(Loc, FilePath)@ on the edge value,
      -- so the refresh is a fold over 'parseAndInsert' — no model
      -- lookup, no @noteSource@ recovery, no @lookupNotesByRoute@.
      --
      -- The dep index keys edges by the path *as written* in
      -- the note-local Lua filter declaration — typically the
      -- layer-relative form like @"filters/x.lua"@, with no mount-point
      -- prefix. unionmount delivers 'fp' in the mounted form (it's the
      -- @Change@ map's outer key — see @changeInsert@ in
      -- @System.UnionMount@). To recover the declaration form we
      -- additionally try stripping each layer's mount-point prefix.
      let candidates = depKeyCandidates layers fp
          dependents =
            Map.unions
              [ SDeps.dependentsOnLua k (model ^. modelSourceDependencies)
              | k <- candidates
              ]
      if Map.null dependents
        then do
          logD $ "Lua filter changed but no notes depend on it: " <> toText fp
          pure id
        else do
          log $ "Lua filter changed (" <> toText fp <> "); re-parsing " <> show (Map.size dependents) <> " dependent note(s)"
          -- Re-parse rewrites the AST, so the cached stork index is stale.
          Stork.clearStorkIndex storkIndexTVar
          chainM (uncurry (parseAndInsert layers noteF model UM.Update)) (Map.toList dependents)
    R.Yaml ->
      case R.mkLmlRouteFromFilePath fp of
        Nothing ->
          pure id
        Just r -> case action of
          UM.Refresh refreshAction overlays -> do
            yamlContents <- forM (NEL.reverse overlays) $ \overlay -> do
              let fpAbs = locResolve overlay
              traverseToSnd (readRefreshedFile refreshAction) fpAbs
            -- A failed parse is no longer an exception (which used to kill
            -- the UnionMount change handler — issue #285); it lives on the
            -- `Left` side of the inserted SData's `_sdataValue` and gets
            -- surfaced at render time.
            let sData = SD.parseSDataCascading r yamlContents
            whenLeft_ (sData ^. SD.sdataValue) $ \err ->
              logE $ "Bad YAML file: " <> err
            pure $ M.modelInsertData sData
          UM.Delete -> do
            log $ "Removing data: " <> toText fp
            pure $ M.modelDeleteData r
    R.HeistTpl ->
      case action of
        UM.Refresh refreshAction overlays -> do
          let fpAbs = locResolve $ head overlays
              -- Once we start loading HTML templates, mark the model as "ready"
              -- so Ema will begin rendering content in place of "Loading..."
              -- indicator
              readyOnTemplates = bool id M.modelReadyForView (refreshAction == UM.Existing)
          act <- do
            s <- readRefreshedFile refreshAction fpAbs
            logD $ "Read " <> show (BS.length s) <> " bytes of template"
            pure $ M.modelHeistTemplate %~ T.addTemplateFile fpAbs fp s
          pure $ readyOnTemplates >>> act
        UM.Delete -> do
          log $ "Removing template: " <> toText fp
          pure $ M.modelHeistTemplate %~ T.removeTemplateFile fp
    R.AnyExt ->
      pure id
  staticPatch <- patchStaticFileIndex fpType fp action
  pure $ sourcePatch >>> staticPatch

{- | Project a source-file change into the static-file index when that source
file should also be addressable by wikilinks or embeds.

The structural branches in 'patchModel'' update their own model state first:
notes, YAML cascades, Heist templates, and Lua filter dependents. Some of those
same source files are also useful as browsable source files. Keeping this as a
second, uniform projection makes the policy explicit and avoids duplicating the
same insert/delete logic in every file-type branch.
-}
patchStaticFileIndex ::
  (MonadIO m, MonadLogger m) =>
  R.FileType R.SourceExt ->
  FilePath ->
  UM.FileAction (NonEmpty (Loc, FilePath)) ->
  m (ModelEma -> ModelEma)
patchStaticFileIndex fpType fp action
  | not $ indexesAsStaticFile fpType = pure id
  | otherwise =
      maybe (pure id) patch
        $ R.mkRouteFromFilePath @_ @'R.AnyExt fp
  where
    patch r = case action of
      UM.Refresh refreshAction overlays -> do
        let fpAbs = locResolve $ head overlays
        doesDirectoryExist fpAbs >>= \case
          True ->
            -- A directory got added; this is not a static 'file'
            pure id
          False -> do
            let logF = case refreshAction of
                  UM.Existing -> logD . ("Registering" <>)
                  _ -> log . ("Re-registering" <>)
            logF $ " file: " <> toText fpAbs <> " " <> show r
            insertStaticFile refreshAction overlays r
      UM.Delete ->
        pure $ M.modelDeleteStaticFile r

indexesAsStaticFile :: R.FileType R.SourceExt -> Bool
indexesAsStaticFile = \case
  R.LMLType _ -> False
  R.LuaFilter -> True
  R.Yaml -> True
  R.HeistTpl -> True
  R.AnyExt -> True

insertStaticFile ::
  (MonadIO m, MonadLogger m) =>
  UM.RefreshAction ->
  NonEmpty (Loc, FilePath) ->
  R.R 'R.AnyExt ->
  m (ModelEma -> ModelEma)
insertStaticFile refreshAction overlays r = do
  let fpAbs = locResolve $ head overlays
  t <- liftIO getCurrentTime
  mInfo <- readStaticFileInfo fpAbs (fmap decodeUtf8 . readRefreshedFile refreshAction)
  pure $ M.modelInsertStaticFile t r fpAbs mInfo

{- | Declaration-form keys an unionmount-delivered path could correspond
to: the path itself, plus each layer-mount-prefix-stripped variant. The
dep index stores the form the user wrote in a note-local Lua filter
declaration (no mount prefix), so a delivered path like
@"sub/filters/x.lua"@ from a layer mounted at @sub@ has to round-trip
back to @"filters/x.lua"@ for the lookup to hit. The fp itself is
included so single-layer-no-mount notebooks still hit on the first try.
-}
depKeyCandidates :: Set Loc -> FilePath -> [FilePath]
depKeyCandidates layers fp =
  fp : mapMaybe stripMP (toList layers)
  where
    stripMP loc = do
      mp <- locMountPoint loc
      List.stripPrefix (mp <> "/") fp

{- | Read a Markdown source from disk, parse it, and produce a transformer
that inserts the note and refreshes its filter-dependency edges. Shared
between the LML refresh path (initial scan + edits) and the LuaFilter
hot-reload path, which both end in "read → parseNote → insert".
-}
parseAndInsert ::
  (MonadIO m, MonadLogger m) =>
  Set Loc ->
  (N.Note -> N.Note) ->
  ModelEma ->
  UM.RefreshAction ->
  R.LMLRoute ->
  (Loc, FilePath) ->
  m (ModelEma -> ModelEma)
parseAndInsert layers noteF model refreshAction r src = do
  s <- readRefreshedFile refreshAction (locResolve src)
  N.ParseResult {N.parsedNote = note, N.luaFilterDeps = filterPaths} <-
    N.parseNote (model ^. M.modelScriptingEngine) (fst . locPath <$> Set.toAscList layers) r src (decodeUtf8 s)
  pure
    $ M.modelInsertNote (noteF note)
    >>> (modelSourceDependencies %~ SDeps.setLuaDeps r src filterPaths)

readRefreshedFile :: (MonadLogger m, MonadIO m) => UM.RefreshAction -> FilePath -> m ByteString
readRefreshedFile refreshAction fp =
  case refreshAction of
    UM.Existing -> do
      logD $ "Loading file: " <> toText fp
      readFileBS fp
    _ ->
      readFileFollowingFsnotify fp

{- | Like `readFileBS` but accounts for file truncation due to us responding
 *immediately* to a fsnotify modify event (which is triggered even before the
 writer *finishes* writing the new contents). We solve this "glitch" by
 delaying the read retry, expecting (hoping really) that *this time* the new
 non-empty contents will come through. 'tis a bit of a HACK though.
-}
readFileFollowingFsnotify :: (MonadIO m, MonadLogger m) => FilePath -> m ByteString
readFileFollowingFsnotify fp = do
  log $ "Reading file: " <> toText fp
  readFileBS fp >>= \case
    "" ->
      reReadFileBS 100 fp >>= \case
        "" ->
          -- Sometimes 100ms is not enough (eg: on WSL), so wait a bit more and
          -- give it another try.
          reReadFileBS 300 fp
        s -> pure s
    s -> pure s
  where
    -- Wait before reading, logging the given delay.
    reReadFileBS ms filePath = do
      threadDelay $ 1000 * ms
      log $ "Re-reading (" <> show ms <> "ms" <> ") file: " <> toText filePath
      readFileBS filePath
