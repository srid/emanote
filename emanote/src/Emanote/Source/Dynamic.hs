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
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.UUID.V4 qualified as UUID
import Ema (Dynamic (..))
import Ema.CLI qualified
import Emanote.CLI qualified as CLI
import Emanote.Model.Note (Note)
import Emanote.Model.Note qualified as N
import Emanote.Model.StaticFile qualified as SF
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
import Optics.Core ((%~), (.~), (^.))
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

{- | The two pieces of mutable state the hot-reload pipeline holds:
the current per-layer patterns and the ledger of events emanote
modified under those patterns. Kept together in one record (and
behind one 'TVar' below) so the pattern table and the suppression
ledger move as a single unit — a pattern change that walks the
ledger sees a consistent snapshot, and a future invariant linking
the two has one place to live.
-}
data IgnoreSlice = IgnoreSlice
  { _slicePatterns :: Map Loc [FilePattern]
  , _sliceModified :: Map FilePath ModifiedEvent
  }

{- | Handle for the hot-reload pipeline's mutable state. Single 'TVar'
keeps the two coupled fields atomic; 'snapshotSlice' and 'updateSlice'
are the only sanctioned accessors.
-}
newtype IgnoreState = IgnoreState
  { _isSlice :: TVar IgnoreSlice
  }

makeLenses ''ModifiedEvent

makeLenses ''IgnoreSlice

makeLenses ''IgnoreState

{- | Session-stable handler context — the values 'emanoteSiteInput'
binds once and threads through every per-change function. Kept in one
record so a new helper that needs (say) 'storkIndex' alongside
'ignoreState' doesn't have to grow another positional argument.
-}
data HandlerCtx = HandlerCtx
  { _ctxIgnoreState :: IgnoreState
  , _ctxLayers :: Set Loc
  , _ctxNoteFn :: Note -> Note
  , _ctxStorkIndex :: Stork.IndexVar
  }

makeLenses ''HandlerCtx

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
  ignoreState <- liftIO $ IgnoreState <$> newTVarIO (IgnoreSlice perLayerIgnore Map.empty)
  let
    ctx =
      HandlerCtx
        { _ctxIgnoreState = ignoreState
        , _ctxLayers = layers
        , _ctxNoteFn = _emanoteConfigNoteFn
        , _ctxStorkIndex = storkIndex
        }
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
          then handleIgnoreFileChanges ctx m0
          else pure m0
      patterns <- _slicePatterns <$> snapshotSlice ignoreState
      foldlM (applyFiltered ctx patterns) m1 (changeEntries restCh)
  Dynamic
    <$> UM.unionMountStreaming
      (layers & Set.map (id &&& Loc.locPath))
      Pattern.filePatterns
      universalForEveryLayer
      initialModel
      handle

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
  HandlerCtx ->
  Map Loc [FilePattern] ->
  Model.ModelEma ->
  (R.FileType R.SourceExt, FilePath, UM.FileAction (NonEmpty (Loc, FilePath))) ->
  m Model.ModelEma
applyFiltered ctx patterns m (fpType, fp, action) = case action of
  UM.Refresh refr overlays -> case Ignore.classifyOverlays patterns overlays of
    Ignore.OverlayKept -> do
      forgetModified (ctx ^. ctxIgnoreState) fp
      dispatch (UM.Refresh refr overlays)
    Ignore.OverlayPartial survivors -> do
      recordModified (ctx ^. ctxIgnoreState) fp (ModifiedEvent fpType overlays refr)
      dispatch (UM.Refresh refr survivors)
    Ignore.OverlayDropped -> do
      recordModified (ctx ^. ctxIgnoreState) fp (ModifiedEvent fpType overlays refr)
      dispatch UM.Delete
  UM.Delete -> do
    forgetModified (ctx ^. ctxIgnoreState) fp
    dispatch UM.Delete
  where
    dispatch a = applyOne ctx m (fpType, fp, a)

applyOne ::
  (MonadUnliftIO m, MonadLoggerIO m) =>
  HandlerCtx ->
  Model.ModelEma ->
  (R.FileType R.SourceExt, FilePath, UM.FileAction (NonEmpty (Loc, FilePath))) ->
  m Model.ModelEma
applyOne ctx m (fpType, fp, action) = do
  trans <- Patch.patchModel (ctx ^. ctxLayers) (ctx ^. ctxNoteFn) (ctx ^. ctxStorkIndex) m fpType fp action
  pure $! trans m

snapshotSlice :: (MonadIO m) => IgnoreState -> m IgnoreSlice
snapshotSlice st = readTVarIO (st ^. isSlice)

updateSlice :: (MonadIO m) => IgnoreState -> (IgnoreSlice -> IgnoreSlice) -> m ()
updateSlice st f =
  liftIO $ atomically $ modifyTVar' (st ^. isSlice) f

{- | Overwrite-wins. Called from 'applyFiltered'; the latest unionmount
  event carries the freshest overlay info so a replacement is correct.
-}
recordModified :: (MonadIO m) => IgnoreState -> FilePath -> ModifiedEvent -> m ()
recordModified st fp ev =
  updateSlice st $ sliceModified %~ Map.insert fp ev

{- | Existing-wins. Called from the model walk in 'evictNewlyIgnored';
  the model only knows the top overlay via @_noteSource@ /
  @_staticFileSource@, so any existing ledger entry (from a prior
  'OverlayPartial') has richer multi-layer history and must survive.
-}
recordModifiedIfAbsent :: (MonadIO m) => IgnoreState -> FilePath -> ModifiedEvent -> m ()
recordModifiedIfAbsent st fp ev =
  updateSlice st $ sliceModified %~ Map.insertWith (\_new old -> old) fp ev

forgetModified :: (MonadIO m) => IgnoreState -> FilePath -> m ()
forgetModified st fp =
  updateSlice st $ sliceModified %~ Map.delete fp

{- | Handle a batch of @.emanoteignore@ events by reloading every layer's
patterns from disk and walking the model + modified-event set so the
hot-reload effect is visible immediately.

Reloading from disk uniformly (rather than diffing the @Change@ map's
per-event overlay entries) lets a 'UM.Delete' of an ignore file Just
Work: the deletion is recorded as a missing file by 'readIgnoreFile',
which collapses to "no patterns for this layer". No per-event
inspection needed.

This intercept runs at the streaming-handler level, *before* events
reach 'Patch.patchModel'. The symmetric hot-reload of Lua filters is
asymmetric on purpose — it lives inside 'Patch.patchModel' because
filter edits invalidate a set of dependents recoverable from the
running model, whereas @.emanoteignore@ edits reshape what belongs in
the model in the first place. See the @R.IgnoreFile@ branch of
'Patch.patchModel' for the reciprocal note.
-}
handleIgnoreFileChanges ::
  (MonadUnliftIO m, MonadLoggerIO m) =>
  HandlerCtx ->
  Model.ModelEma ->
  m Model.ModelEma
handleIgnoreFileChanges ctx m0 = do
  let st = ctx ^. ctxIgnoreState
  oldPatterns <- _slicePatterns <$> snapshotSlice st
  newPatterns <- Ignore.loadIgnorePatterns (ctx ^. ctxLayers)
  if newPatterns == oldPatterns
    then do
      logD "Hot-reload: .emanoteignore event with no pattern change"
      pure m0
    else do
      updateSlice st $ slicePatterns .~ newPatterns
      log "Hot-reload: .emanoteignore patterns changed; refreshing model"
      m1 <- evictNewlyIgnored ctx newPatterns m0
      reEmitModified ctx newPatterns m1

{- | For each note or static file whose source now matches a pattern
that didn't exist before, delete it from the model and remember the
event so a later pattern relaxation can resurrect it.

Notes are walked via @_modelNotes@; static files via
@_modelStaticFiles@. Auto-generated ancestor placeholders carry
@_noteSource = Nothing@ and are skipped, since they aren't files on
disk and can't be ignored. YAML data and Heist templates carry no
source metadata yet — see the docs note in
@docs/guide/emanoteignore.md@.
-}
evictNewlyIgnored ::
  (MonadUnliftIO m, MonadLoggerIO m) =>
  HandlerCtx ->
  Map Loc [FilePattern] ->
  Model.ModelEma ->
  m Model.ModelEma
evictNewlyIgnored ctx newPatterns m0 = do
  let noteVictims =
        [ (note, loc, lfp)
        | note <- Ix.toList (m0 ^. Model.modelNotes)
        , Just (loc, lfp) <- pure (N._noteSource note)
        , Ignore.isLayerPathIgnored newPatterns loc lfp
        ]
      staticVictims =
        [ (sf, loc, lfp)
        | sf <- Ix.toList (m0 ^. Model.modelStaticFiles)
        , Just (loc, lfp) <- pure (SF._staticFileSource sf)
        , Ignore.isLayerPathIgnored newPatterns loc lfp
        ]
  unless (null noteVictims) $ Stork.clearStorkIndex (ctx ^. ctxStorkIndex)
  m1 <- foldlM evictNote m0 noteVictims
  foldlM evictStatic m1 staticVictims
  where
    evictNote m (note, loc, lfp) = do
      let route = N._noteRoute note
          fpType = lmlRouteFileType route
          fpMounted = mountedPath loc lfp
      log $ "Hot-reload: hiding note " <> toText fpMounted
      -- ifAbsent: a previous OverlayPartial may have stashed the full
      -- multi-layer overlay; the model walk only sees one layer via
      -- _noteSource, so an unconditional insert would lose that history.
      recordModifiedIfAbsent (ctx ^. ctxIgnoreState) fpMounted (ModifiedEvent fpType ((loc, lfp) :| []) UM.Existing)
      pure $! Model.modelDeleteNote route m
    evictStatic m (sf, loc, lfp) = do
      let route = SF._staticFileRoute sf
          fpMounted = mountedPath loc lfp
      log $ "Hot-reload: hiding static file " <> toText fpMounted
      -- R.AnyExt is the tag unionmount classifies static files under, so
      -- a future re-emit through patchModel routes correctly. ifAbsent
      -- preserves any richer overlay history from a prior partial filter
      -- (see the matching evictNote branch for the same constraint).
      recordModifiedIfAbsent (ctx ^. ctxIgnoreState) fpMounted (ModifiedEvent R.AnyExt ((loc, lfp) :| []) UM.Existing)
      pure $! Model.modelDeleteStaticFile route m

{- | For every event we previously suppressed or trimmed, re-evaluate it
under the new patterns and emit the appropriate action via
'Patch.patchModel'. Files that are now fully unignored are removed
from the tracking set; those still hidden or partially trimmed stay
recorded for the next pattern change.
-}
reEmitModified ::
  (MonadUnliftIO m, MonadLoggerIO m) =>
  HandlerCtx ->
  Map Loc [FilePattern] ->
  Model.ModelEma ->
  m Model.ModelEma
reEmitModified ctx newPatterns m0 = do
  let st = ctx ^. ctxIgnoreState
  entries <- Map.toList . _sliceModified <$> snapshotSlice st
  foldlM (reEmit st) m0 entries
  where
    reEmit st m (fp, ev) =
      let overlays = ev ^. meOriginalOverlays
          fpType = ev ^. meType
          refr = ev ^. meRefreshAction
          dispatch a = applyOne ctx m (fpType, fp, a)
       in case Ignore.classifyOverlays newPatterns overlays of
            Ignore.OverlayDropped ->
              pure m
            Ignore.OverlayKept -> do
              forgetModified st fp
              log $ "Hot-reload: re-including " <> toText fp
              dispatch (UM.Refresh refr overlays)
            Ignore.OverlayPartial survivors -> do
              recordModified st fp (ModifiedEvent fpType overlays refr)
              log $ "Hot-reload: re-including " <> toText fp <> " (partial overlay)"
              dispatch (UM.Refresh refr survivors)

mountedPath :: Loc -> FilePath -> FilePath
mountedPath loc lfp =
  maybe lfp (</> lfp) (Loc.locMountPoint loc)

{- | The unionmount tag for an LML route. 'MR.withLmlRoute' selects the
  right 'HasExt' instance, so 'fileType' returns 'R.LMLType R.Md' or
  'R.LMLType R.Org' without a manual case-split.
-}
lmlRouteFileType :: LMLRoute -> R.FileType R.SourceExt
lmlRouteFileType = MR.withLmlRoute (\(_ :: R.R ('R.LMLType lml)) -> R.fileType @_ @('R.LMLType lml))

{- | Does an 'R.IgnoreFile' event involve a layer-root @.emanoteignore@?
Sub-tree files share the tag but configure no pattern set, so a batch
that contains only sub-tree events can skip the reload entirely.
-}
isRootedIgnoreFile :: UM.FileAction (NonEmpty (Loc, FilePath)) -> Bool
isRootedIgnoreFile = \case
  UM.Refresh _ overlays -> any (Ignore.isLayerRootIgnoreFile . snd) overlays
  UM.Delete -> True

makeLenses ''EmanoteConfig
