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
import Data.Set qualified as Set
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Emanote.Model qualified as M
import Emanote.Model.Note qualified as N
import Emanote.Model.SData qualified as SD
import Emanote.Model.SourceDependencies qualified as SDeps
import Emanote.Model.StaticFile (readStaticFileInfo)
import Emanote.Model.Stork.Index qualified as Stork
import Emanote.Model.Type (ModelEma, modelNotes, modelSourceDependencies)
import Emanote.Prelude (
  chainM,
  log,
  logD,
  logE,
  logW,
 )
import Emanote.Route qualified as R
import Emanote.Source.Loc (Loc, locMountPoint, locResolve, userLayersToSearch)
import Emanote.Source.Pattern (filePatterns, ignorePatterns)
import Heist.Extra.TemplateState qualified as T
import Optics.Operators ((%~), (^.))
import Relude
import Relude.Extra (traverseToSnd)
import System.UnionMount qualified as UM
import Text.Pandoc.Scripting (ScriptingEngine)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Directory (doesDirectoryExist, doesFileExist)

{- | Map a filesystem change to the corresponding model change.

The streaming handler in 'Source.Dynamic' hands every per-file step
the running model; we forward it uniformly so 'patchModel' has one
shape across all file types. Only the 'R.LuaFilter' branch reads it
today (to walk the reverse-dependency index in
@Emanote.Model.SourceDependencies@). The returned transformer is
applied to that same model by the caller.
-}
patchModel ::
  (MonadIO m, MonadLogger m, MonadLoggerIO m) =>
  Set Loc ->
  (N.Note -> N.Note) ->
  Stork.IndexVar ->
  ScriptingEngine ->
  ModelEma ->
  R.FileType R.SourceExt ->
  FilePath ->
  UM.FileAction (NonEmpty (Loc, FilePath)) ->
  m (ModelEma -> ModelEma)
patchModel layers noteF storkIndexTVar scriptingEngine model fpType fp action = do
  logger <- askLoggerIO
  now <- liftIO getCurrentTime
  -- Prefix all patch logging with timestamp.
  let newLogger loc src lvl s =
        logger loc src lvl $ fromString (formatTime defaultTimeLocale "[%H:%M:%S] " now) <> s
  runLoggingT (patchModel' layers noteF storkIndexTVar scriptingEngine model fpType fp action) newLogger

patchModel' ::
  (MonadIO m, MonadLogger m) =>
  Set Loc ->
  (N.Note -> N.Note) ->
  Stork.IndexVar ->
  ScriptingEngine ->
  ModelEma ->
  R.FileType R.SourceExt ->
  FilePath ->
  UM.FileAction (NonEmpty (Loc, FilePath)) ->
  m (ModelEma -> ModelEma)
patchModel' layers noteF storkIndexTVar scriptingEngine model fpType fp action = do
  case fpType of
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
              parseAndInsert layers noteF scriptingEngine refreshAction r (head overlays)
            UM.Delete -> do
              log $ "Removing note: " <> toText fp
              pure $ M.modelDeleteNote r
    R.LuaFilter -> do
      -- An edit (or deletion) of a Pandoc Lua filter file invalidates
      -- every note that referenced it at parse time. We re-read those
      -- notes from disk and re-run 'parseNote' so their cached AST
      -- reflects the new (or now-missing) filter. Dependents are
      -- looked up directly in the live model — that's the read-model
      -- capability 'unionMountStreaming' provides.
      --
      -- The dep index keys edges by the path *as written* in
      -- @pandoc.filters@ frontmatter — typically the layer-relative
      -- form like @"filters/x.lua"@, with no mount-point prefix.
      -- unionmount delivers 'fp' in the mounted form (it's the
      -- @Change@ map's outer key — see @changeInsert@ in
      -- @System.UnionMount@). To recover the frontmatter form we
      -- additionally try stripping each layer's mount-point prefix.
      let candidates = depKeyCandidates layers fp
          dependents =
            mconcat
              [ SDeps.dependentsOnLua k (model ^. modelSourceDependencies)
              | k <- candidates
              ]
      let depList = Set.toList dependents
      if null depList
        then do
          logD $ "Lua filter changed but no notes depend on it: " <> toText fp
          pure id
        else do
          log $ "Lua filter changed (" <> toText fp <> "); re-parsing " <> show (length depList) <> " dependent note(s)"
          -- Re-parse rewrites the AST, so the cached stork index is stale.
          Stork.clearStorkIndex storkIndexTVar
          chainM (reparseOrDropNote layers noteF scriptingEngine model) depList
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
    R.AnyExt -> do
      case R.mkRouteFromFilePath fp of
        Nothing ->
          pure id
        Just r -> case action of
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
                t <- liftIO getCurrentTime
                mInfo <- readStaticFileInfo fpAbs (fmap decodeUtf8 . readRefreshedFile refreshAction)
                pure $ M.modelInsertStaticFile t r fpAbs mInfo
          UM.Delete -> do
            pure $ M.modelDeleteStaticFile r

{- | Frontmatter-form keys an unionmount-delivered path could correspond
to: the path itself, plus each layer-mount-prefix-stripped variant. The
dep index stores the form the user wrote in @pandoc.filters@ (no mount
prefix), so a delivered path like @"sub/filters/x.lua"@ from a layer
mounted at @sub@ has to round-trip back to @"filters/x.lua"@ for the
lookup to hit. The fp itself is included so single-layer-no-mount
notebooks still hit on the first try.
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
  ScriptingEngine ->
  UM.RefreshAction ->
  R.LMLRoute ->
  (Loc, FilePath) ->
  m (ModelEma -> ModelEma)
parseAndInsert layers noteF scriptingEngine refreshAction r src = do
  s <- readRefreshedFile refreshAction (locResolve src)
  N.ParseResult {N.parsedNote = note, N.luaFilterDeps = filterPaths} <-
    N.parseNote scriptingEngine (userLayersToSearch layers) r src (decodeUtf8 s)
  pure
    $ M.modelInsertNote (noteF note)
    >>> (modelSourceDependencies %~ SDeps.setLuaDeps r filterPaths)

{- | Re-read a note's Markdown source and produce a transformer that
replaces the cached note. The @OrDrop@ suffix names a side effect
the function name alone wouldn't telegraph: if the note no longer
exists in the model, this drops the stale entry from
'modelSourceDependencies' instead of erroring or silently doing
nothing — callers reaching for "just re-read this note" need to
know the dependency index also gets pruned. Synthetic notes (no
on-disk source) are a no-op.
-}
reparseOrDropNote ::
  (MonadIO m, MonadLogger m) =>
  Set Loc ->
  (N.Note -> N.Note) ->
  ScriptingEngine ->
  ModelEma ->
  R.LMLRoute ->
  m (ModelEma -> ModelEma)
reparseOrDropNote layers noteF scriptingEngine model depRoute =
  case N.lookupNotesByRoute depRoute (model ^. modelNotes) of
    Nothing -> do
      -- Note disappeared between dependency-edge creation and now;
      -- drop the stale edge. Structurally this shouldn't happen — a
      -- normal note delete cleans the edge via 'modelDeleteNote' — so
      -- warn rather than silently recover.
      logW $ "Stale lua dependency for missing note " <> show depRoute <> "; dropping edge"
      pure (modelSourceDependencies %~ SDeps.removeNote depRoute)
    Just oldNote ->
      case oldNote ^. N.noteSource of
        Nothing ->
          -- Synthesized notes (e.g. ancestor placeholders) have no
          -- on-disk source; nothing to re-read.
          pure id
        Just src@(_, srcFp) -> do
          let srcAbs = locResolve src
          -- A batch can contain both a '.lua' change and a '.md'
          -- deletion on the dependent note. Reading the missing
          -- file would propagate an IOException up to unionmount's
          -- 'interceptExceptions' and lose the entire batch's update.
          -- Guard the read and drop the stale edge if the source has
          -- already disappeared.
          doesFileExist srcAbs >>= \case
            False -> do
              logW $ "Lua dep target " <> toText srcFp <> " is gone; dropping edge for " <> show depRoute
              pure (modelSourceDependencies %~ SDeps.removeNote depRoute)
            True -> do
              result <- parseAndInsert layers noteF scriptingEngine UM.Update depRoute src
              logD $ "Re-parsed " <> toText srcFp <> " after lua filter change"
              pure result

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
