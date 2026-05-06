-- | Patch model state depending on file change event.
module Emanote.Source.Patch (
  patchModel,
  filePatterns,
  ignorePatterns,
) where

import Control.Monad.Logger (LoggingT (runLoggingT), MonadLogger, MonadLoggerIO (askLoggerIO))
import Data.ByteString qualified as BS
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
  log,
  logD,
  logE,
 )
import Emanote.Route qualified as R
import Emanote.Source.Loc (Loc, locResolve, userLayersToSearch)
import Emanote.Source.Pattern (filePatterns, ignorePatterns)
import Heist.Extra.TemplateState qualified as T
import Optics.Operators ((%~), (^.))
import Relude
import Relude.Extra (traverseToSnd)
import System.FilePath ((</>))
import System.UnionMount qualified as UM
import Text.Pandoc.Scripting (ScriptingEngine)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Directory (doesDirectoryExist)

-- | Map a filesystem change to the corresponding model change.
patchModel ::
  (MonadIO m, MonadLogger m, MonadLoggerIO m) =>
  Set Loc ->
  (N.Note -> N.Note) ->
  Stork.IndexVar ->
  -- | Lua scripting engine
  ScriptingEngine ->
  -- | Snapshot of the model from before this batch of changes. Read by
  -- handlers (e.g. 'LuaFilter') that must inspect existing state to
  -- compute their effect; never mutated here.
  TVar ModelEma ->
  -- | Type of the file being changed
  R.FileType R.SourceExt ->
  -- | Path to the file being changed
  FilePath ->
  -- | Specific change to the file, along with its paths from other "layers"
  UM.FileAction (NonEmpty (Loc, FilePath)) ->
  m (ModelEma -> ModelEma)
patchModel layers noteF storkIndexTVar scriptingEngine modelTVar fpType fp action = do
  logger <- askLoggerIO
  now <- liftIO getCurrentTime
  -- Prefix all patch logging with timestamp.
  let newLogger loc src lvl s =
        logger loc src lvl $ fromString (formatTime defaultTimeLocale "[%H:%M:%S] " now) <> s
  runLoggingT (patchModel' layers noteF storkIndexTVar scriptingEngine modelTVar fpType fp action) newLogger

-- | Map a filesystem change to the corresponding model change.
patchModel' ::
  (MonadIO m, MonadLogger m) =>
  Set Loc ->
  (N.Note -> N.Note) ->
  Stork.IndexVar ->
  -- | Lua scripting engine
  ScriptingEngine ->
  TVar ModelEma ->
  -- | Type of the file being changed
  R.FileType R.SourceExt ->
  -- | Path to the file being changed
  FilePath ->
  -- | Specific change to the file, along with its paths from other "layers"
  UM.FileAction (NonEmpty (Loc, FilePath)) ->
  m (ModelEma -> ModelEma)
patchModel' layers noteF storkIndexTVar scriptingEngine modelTVar fpType fp action = do
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
            UM.Refresh refreshAction overlays -> do
              let fpAbs = head overlays
              s <- readRefreshedFile refreshAction $ locResolve fpAbs
              note <- N.parseNote scriptingEngine (userLayersToSearch layers) r fpAbs (decodeUtf8 s)
              pure $ M.modelInsertNote $ noteF note
            UM.Delete -> do
              log $ "Removing note: " <> toText fp
              pure $ M.modelDeleteNote r
    R.LuaFilter -> do
      -- A change (or deletion) of a Pandoc Lua filter file invalidates
      -- every note that referenced this filter at parse time. We re-read
      -- those notes from disk and re-run 'parseNote' so their cached
      -- 'Pandoc' AST reflects the new (or now-missing) filter. The set
      -- of dependents is read from the pre-batch model snapshot in
      -- 'modelTVar'; the snapshot is current enough because the per-batch
      -- writer in 'Emanote.Source.Dynamic' updates it before the next
      -- batch runs.
      --
      -- 'Refresh' carries the resolved layer overlay so we can compute
      -- the absolute filter path directly. 'Delete' does not, so we try
      -- the deleted relative path against every layer and union the
      -- resulting dependent sets — the false-positive direction is
      -- benign (a re-parse just yields the correct AST again).
      --
      -- The candidate-path expansion assumes the layer list is stable
      -- across the lifetime of a single delete event, which is currently
      -- the case (layers are fixed at startup; live layer-swap is not a
      -- supported feature). If that ever changes, candidates derived from
      -- a stale layer set could miss a dependent.
      model <- readTVarIO modelTVar
      let candidatePaths = case action of
            UM.Refresh _ overlays -> one (locResolve (head overlays))
            UM.Delete -> luaFilterAbsPathsFor layers fp
          dependents =
            mconcat
              [ SDeps.dependentsOnLua p (model ^. modelSourceDependencies)
              | p <- candidatePaths
              ]
      if Set.null dependents
        then do
          logD $ "Lua filter changed but no notes depend on it: " <> toText fp
          pure id
        else do
          log $ "Lua filter changed (" <> toText fp <> "); re-parsing " <> show (Set.size dependents) <> " dependent note(s)"
          foldr (>>>) id <$> traverse (reparseOrDropNote layers noteF scriptingEngine model) (Set.toList dependents)
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

{- | Absolute paths to attempt when a 'LuaFilter' file is reported deleted.
We don't know which layer it lived in, so try each user layer; lookups
against the dependency index will only match the layers that actually
registered an edge for this path.
-}
luaFilterAbsPathsFor :: Set Loc -> FilePath -> [FilePath]
luaFilterAbsPathsFor layers fp =
  userLayersToSearch layers <&> (</> fp)

{- | Re-read a note's Markdown source from disk and produce a transformer
that replaces the cached note in the model with the freshly parsed one.

If the note no longer exists in the model, this drops the stale entry
from 'modelSourceDependencies' instead — the function name's "OrDrop"
suffix names that side effect, since callers reaching for "just re-read
this note" should know the dependency index also gets pruned. Synthetic
notes with no on-disk source are a no-op.
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
    Nothing ->
      -- Note disappeared between dependency-edge creation and now;
      -- drop the stale edge.
      pure (modelSourceDependencies %~ SDeps.removeNote depRoute)
    Just oldNote ->
      case oldNote ^. N.noteSource of
        Nothing ->
          -- Synthesized notes (e.g. ancestor placeholders) have no
          -- on-disk source; nothing to re-read.
          pure id
        Just src@(_, srcFp) -> do
          let srcAbs = locResolve src
          s <- readRefreshedFile UM.Update srcAbs
          note <- N.parseNote scriptingEngine (userLayersToSearch layers) depRoute src (decodeUtf8 s)
          logD $ "Re-parsed " <> toText srcFp <> " after lua filter change"
          pure $ M.modelInsertNote $ noteF note

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
