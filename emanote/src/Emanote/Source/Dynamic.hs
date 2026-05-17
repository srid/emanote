{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Emanote.Source.Dynamic (
  emanoteSiteInput,
  EmanoteConfig (..),
  emanoteCompileTailwind,
  emanoteConfigCli,
  emanoteConfigNoteFn,
  emanoteConfigPandocRenderers,
) where

import Control.Monad.Logger (MonadLoggerIO)
import Data.IxSet.Typed qualified as Ix
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.UUID.V4 qualified as UUID
import Ema (Dynamic (..))
import Ema.CLI qualified
import Emanote.CLI qualified as CLI
import Emanote.Model.Note (Note)
import Emanote.Model.Note qualified as N
import Emanote.Model.Stork.Index qualified as Stork
import Emanote.Model.Type qualified as Model
import Emanote.Pandoc.Renderer (EmanotePandocRenderers)
import Emanote.Prelude (log, logD)
import Emanote.Route (LMLRoute)
import Emanote.Route qualified as R
import Emanote.Route.ModelRoute qualified as MR
import Emanote.Source.Ignore qualified as Ignore
import Emanote.Source.Loc (Loc)
import Emanote.Source.Loc qualified as Loc
import Emanote.Source.Patch qualified as Patch
import Emanote.Source.Pattern qualified as Pattern
import Optics.Core ((^.))
import Optics.TH (makeLenses)
import Paths_emanote qualified
import Relude
import System.FilePath ((</>))
import System.FilePattern (FilePattern)
import System.UnionMount qualified as UM
import Text.Pandoc.Lua (getEngine)
import UnliftIO (MonadUnliftIO)

-- | Everything that's required to run an Emanote site.
data EmanoteConfig = EmanoteConfig
  { _emanoteConfigCli :: CLI.Cli
  -- ^ CLI arguments (includes layers)
  , _emanoteConfigNoteFn :: Note -> Note
  -- ^ A function to filter the `Note` before it gets added to the model.
  , _emanoteConfigPandocRenderers :: EmanotePandocRenderers Model.Model LMLRoute
  -- ^ How to render Pandoc to Heist HTML.
  , _emanoteCompileTailwind :: Bool
  -- ^ Whether to replace Tailwind2 CDN with a minimized Tailwind3 CSS file.
  }

{- | An event whose overlays were trimmed (or wholly suppressed) by the
current per-layer ignore patterns. Captured verbatim so that when those
patterns later change we can re-emit the event without a fresh disk walk.
-}
data ModifiedEvent = ModifiedEvent
  { _meType :: R.FileType R.SourceExt
  , _meOriginalOverlays :: NonEmpty (Loc, FilePath)
  , _meRefreshAction :: UM.RefreshAction
  -- ^ Preserved so a resurrect uses the same @RefreshAction@ the
  --   original event carried (e.g. 'UM.Existing' for an initial-scan
  --   file vs. 'UM.Update' for a mid-session modify).
  }

{- | Runtime state for the @.emanoteignore@ hot-reload pipeline.

The 'TVar' lets a writer-side fsnotify thread update patterns while
the reader-side overlay filter reads them — both stay coordinated via
STM. Tracking the modified events as a sibling field keeps the
"what's currently hidden / trimmed" set queryable without re-deriving
it from the live model on each pattern change.
-}
data IgnoreState = IgnoreState
  { _isPatterns :: TVar (Map Loc [FilePattern])
  , _isModifiedEvents :: TVar (Map FilePath ModifiedEvent)
  }

{- | Make an Ema `Dynamic` for the Emanote model. The bulk of logic
for building the Dynamic is in @Source/Patch.hs@.

We use 'UM.unionMountStreaming' so per-file closures are GC'd as
their updates land (caps initial-load memory on large notebooks —
@srid/emanote#66@) and so 'Patch.patchModel' can read the running
model when walking the Lua-filter dep index (@srid/emanote#263@).

Per-layer @.emanoteignore@ patterns are applied inside the streaming
handler rather than pushed to unionmount: the patterns live in a
'TVar' that the @.emanoteignore@ change events update in place, so a
mid-session edit takes effect immediately (issue @srid/emanote#739@).
-}
emanoteSiteInput :: (MonadUnliftIO m, MonadLoggerIO m) => Ema.CLI.Action -> EmanoteConfig -> m (Dynamic m Model.ModelEma)
emanoteSiteInput cliAct EmanoteConfig {..} = do
  defaultLayer <- Loc.defaultLayer <$> liftIO Paths_emanote.getDataDir
  instanceId <- liftIO UUID.nextRandom
  storkIndex <- Stork.newIndex
  scriptingEngine <- getEngine
  let layers = Loc.userLayers ((CLI.path &&& CLI.mountPoint) <$> CLI.layers _emanoteConfigCli) <> one defaultLayer
      initialModel =
        Model.emptyModel
          layers
          cliAct
          _emanoteConfigPandocRenderers
          scriptingEngine
          _emanoteCompileTailwind
          (CLI.allowBrokenLuaFilters _emanoteConfigCli)
          instanceId
          storkIndex
  perLayerIgnore <- Ignore.loadIgnorePatterns layers
  ignoreState <-
    liftIO
      $ IgnoreState
      <$> newTVarIO perLayerIgnore
      <*> newTVarIO Map.empty
  let
    -- unionmount sees only the universal patterns (per-source-mapped).
    -- Per-layer @.emanoteignore@ patterns are applied inside 'handle'
    -- so they can hot-reload without tearing the mount down.
    universalForEveryLayer = Map.fromSet (const Pattern.ignorePatterns) layers
    handle ::
      (MonadUnliftIO m, MonadLoggerIO m) =>
      UM.Change Loc (R.FileType R.SourceExt) ->
      Model.ModelEma ->
      m Model.ModelEma
    handle change m0 = do
      let (ignoreCh, restCh) = Map.partitionWithKey (\k _ -> k == R.IgnoreFile) change
          -- Sub-tree `.emanoteignore` events (e.g. @sub/.emanoteignore@)
          -- arrive under the same 'R.IgnoreFile' tag but configure no
          -- pattern set — only the layer-root file is consulted. Skip
          -- the reload entirely when nothing rooted is in the batch.
          ignoreActions = [a | files <- Map.elems ignoreCh, a <- Map.elems files]
          ignoreRooted = any isRootedIgnoreFile ignoreActions
      m1 <-
        if ignoreRooted
          then handleIgnoreFileChanges ignoreState layers _emanoteConfigNoteFn storkIndex m0
          else pure m0
      patterns <- readTVarIO (_isPatterns ignoreState)
      foldlM (applyFiltered ignoreState layers _emanoteConfigNoteFn storkIndex patterns) m1 (changeEntries restCh)
  Dynamic
    <$> UM.unionMountStreaming
      (layers & Set.map (id &&& Loc.locPath))
      Pattern.filePatterns
      universalForEveryLayer
      initialModel
      handle

{- | Flatten a `Change` into per-file entries so a streaming handler can
fold them one at a time (the GC-eager pattern unionMountStreaming
exists for; see its haddock).
-}
changeEntries ::
  UM.Change Loc (R.FileType R.SourceExt) ->
  [(R.FileType R.SourceExt, FilePath, UM.FileAction (NonEmpty (Loc, FilePath)))]
changeEntries ch =
  [ (fpType, fp, action)
  | (fpType, files) <- Map.toList ch
  , (fp, action) <- Map.toList files
  ]

{- | Apply a single file event under the current ignore patterns.

A 'Refresh' whose overlay survivors are empty becomes a 'Delete' and
is recorded so a later pattern relaxation can resurrect it. A
partially trimmed 'Refresh' is dispatched with only the surviving
overlays — that matches the per-source semantics 'unionMount' offers
when it owns the ignore map.
-}
applyFiltered ::
  (MonadUnliftIO m, MonadLoggerIO m) =>
  IgnoreState ->
  Set Loc ->
  (Note -> Note) ->
  Stork.IndexVar ->
  Map Loc [FilePattern] ->
  Model.ModelEma ->
  (R.FileType R.SourceExt, FilePath, UM.FileAction (NonEmpty (Loc, FilePath))) ->
  m Model.ModelEma
applyFiltered st layers noteFn storkIndex patterns m (fpType, fp, action) = case action of
  UM.Refresh refr overlays -> do
    let survivors = NE.filter (\(loc, lfp) -> not (Ignore.isLayerPathIgnored patterns loc lfp)) overlays
        originalCount = NE.length overlays
        survivorCount = length survivors
    case nonEmpty survivors of
      Nothing -> do
        recordModified st fp (ModifiedEvent fpType overlays refr)
        dispatch m (fpType, fp, UM.Delete)
      Just survNE
        | survivorCount == originalCount -> do
            forgetModified st fp
            dispatch m (fpType, fp, UM.Refresh refr overlays)
        | otherwise -> do
            recordModified st fp (ModifiedEvent fpType overlays refr)
            dispatch m (fpType, fp, UM.Refresh refr survNE)
  UM.Delete -> do
    forgetModified st fp
    dispatch m (fpType, fp, UM.Delete)
  where
    dispatch = applyOne layers noteFn storkIndex

-- | Apply one unfiltered event by delegating to 'Patch.patchModel'.
applyOne ::
  (MonadUnliftIO m, MonadLoggerIO m) =>
  Set Loc ->
  (Note -> Note) ->
  Stork.IndexVar ->
  Model.ModelEma ->
  (R.FileType R.SourceExt, FilePath, UM.FileAction (NonEmpty (Loc, FilePath))) ->
  m Model.ModelEma
applyOne layers noteFn storkIndex m (fpType, fp, action) = do
  trans <- Patch.patchModel layers noteFn storkIndex m fpType fp action
  pure $! trans m

recordModified :: (MonadIO m) => IgnoreState -> FilePath -> ModifiedEvent -> m ()
recordModified st fp ev =
  liftIO $ atomically $ modifyTVar' (_isModifiedEvents st) (Map.insert fp ev)

forgetModified :: (MonadIO m) => IgnoreState -> FilePath -> m ()
forgetModified st fp =
  liftIO $ atomically $ modifyTVar' (_isModifiedEvents st) (Map.delete fp)

{- | Handle a batch of @.emanoteignore@ events by reloading every layer's
patterns from disk and walking the model + modified-event set so the
hot-reload effect is visible immediately.

Reloading from disk uniformly (rather than diffing the @Change@ map's
per-event overlay entries) lets a 'UM.Delete' of an ignore file Just
Work: the deletion is recorded as a missing file by 'readIgnoreFile',
which collapses to "no patterns for this layer". No per-event
inspection needed.
-}
handleIgnoreFileChanges ::
  (MonadUnliftIO m, MonadLoggerIO m) =>
  IgnoreState ->
  Set Loc ->
  (Note -> Note) ->
  Stork.IndexVar ->
  Model.ModelEma ->
  m Model.ModelEma
handleIgnoreFileChanges st layers noteFn storkIndex m0 = do
  oldPatterns <- readTVarIO (_isPatterns st)
  newPatterns <- Ignore.loadIgnorePatterns layers
  if newPatterns == oldPatterns
    then do
      logD "Hot-reload: .emanoteignore event with no pattern change"
      pure m0
    else do
      atomically $ writeTVar (_isPatterns st) newPatterns
      log "Hot-reload: .emanoteignore patterns changed; refreshing model"
      m1 <- evictNewlyIgnored st layers storkIndex newPatterns m0
      reEmitModified st layers noteFn storkIndex newPatterns m1

{- | For each note whose top-overlay source now matches a pattern that
didn't exist before, delete it from the model and remember the event so
a later pattern relaxation can resurrect it.

This walks @_modelNotes@ — auto-generated ancestor placeholders carry
@_noteSource = Nothing@ and are skipped, since they aren't files on
disk and can't be ignored.
-}
evictNewlyIgnored ::
  (MonadUnliftIO m, MonadLoggerIO m) =>
  IgnoreState ->
  Set Loc ->
  Stork.IndexVar ->
  Map Loc [FilePattern] ->
  Model.ModelEma ->
  m Model.ModelEma
evictNewlyIgnored st _layers storkIndex newPatterns m0 = do
  let victims =
        [ (note, loc, lfp)
        | note <- Ix.toList (m0 ^. Model.modelNotes)
        , Just (loc, lfp) <- pure (N._noteSource note)
        , Ignore.isLayerPathIgnored newPatterns loc lfp
        ]
  unless (null victims) $ Stork.clearStorkIndex storkIndex
  foldlM evict m0 victims
  where
    evict m (note, loc, lfp) = do
      let route = N._noteRoute note
          fpType = lmlRouteFileType route
          fpMounted = mountedPath loc lfp
      log $ "Hot-reload: hiding " <> toText fpMounted
      recordModified st fpMounted (ModifiedEvent fpType ((loc, lfp) :| []) UM.Existing)
      pure $! Model.modelDeleteNote route m

{- | For every event we previously suppressed or trimmed, re-evaluate it
under the new patterns and emit the appropriate action via
'Patch.patchModel'. Files that are now fully unignored are removed
from the tracking set; those still hidden or partially trimmed stay
recorded for the next pattern change.
-}
reEmitModified ::
  (MonadUnliftIO m, MonadLoggerIO m) =>
  IgnoreState ->
  Set Loc ->
  (Note -> Note) ->
  Stork.IndexVar ->
  Map Loc [FilePattern] ->
  Model.ModelEma ->
  m Model.ModelEma
reEmitModified st layers noteFn storkIndex newPatterns m0 = do
  entries <- Map.toList <$> readTVarIO (_isModifiedEvents st)
  foldlM reEmit m0 entries
  where
    reEmit m (fp, ev) = do
      let overlays = _meOriginalOverlays ev
          fpType = _meType ev
          refr = _meRefreshAction ev
          survivors = NE.filter (\(loc, lfp) -> not (Ignore.isLayerPathIgnored newPatterns loc lfp)) overlays
          originalCount = NE.length overlays
          survivorCount = length survivors
      case nonEmpty survivors of
        Nothing ->
          pure m
        Just survNE
          | survivorCount == originalCount -> do
              forgetModified st fp
              log $ "Hot-reload: re-including " <> toText fp
              applyOne layers noteFn storkIndex m (fpType, fp, UM.Refresh refr overlays)
          | otherwise -> do
              recordModified st fp (ModifiedEvent fpType overlays refr)
              log $ "Hot-reload: re-including " <> toText fp <> " (partial overlay)"
              applyOne layers noteFn storkIndex m (fpType, fp, UM.Refresh refr survNE)

-- | Recover the unionmount-style mounted path from a layer + layer-relative file.
mountedPath :: Loc -> FilePath -> FilePath
mountedPath loc lfp =
  maybe lfp (</> lfp) (Loc.locMountPoint loc)

{- | The unionmount tag for an LML route, used when synthesising an event
  for a note we want to evict.
-}
lmlRouteFileType :: LMLRoute -> R.FileType R.SourceExt
lmlRouteFileType r
  | MR.isMdRoute r = R.LMLType R.Md
  | otherwise = R.LMLType R.Org

{- | Does an 'R.IgnoreFile' event involve a layer-root @.emanoteignore@?
Sub-tree files share the tag but configure no pattern set, so a batch
that contains only sub-tree events can skip the reload entirely.
-}
isRootedIgnoreFile :: UM.FileAction (NonEmpty (Loc, FilePath)) -> Bool
isRootedIgnoreFile = \case
  UM.Refresh _ overlays -> any (Ignore.isLayerRootIgnoreFile . snd) overlays
  UM.Delete -> True

makeLenses ''EmanoteConfig
