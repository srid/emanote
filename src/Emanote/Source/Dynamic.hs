module Emanote.Source.Dynamic
  ( emanoteModelDynamic,
    ChangeHandler,
  )
where

import Control.Monad.Logger (MonadLogger, MonadLoggerIO)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Some (Some)
import Data.UUID.V4 qualified as UUID
import Ema (Dynamic (..))
import Ema.CLI qualified
import Ema.Route.Encoder (RouteEncoder)
import Emanote.CLI qualified as CLI
import Emanote.Model.Type qualified as Model
import Emanote.Prelude (chainM)
import Emanote.Route.Ext (FileType, SourceExt)
import Emanote.Route.SiteRoute.Type (SiteRoute)
import Emanote.Source.Loc (Loc)
import Emanote.Source.Loc qualified as Loc
import Emanote.Source.Patch qualified as Patch
import Emanote.Source.Pattern qualified as Pattern
import Paths_emanote qualified
import Relude
import System.FilePattern (FilePattern)
import System.UnionMount qualified as UM
import UnliftIO (MonadUnliftIO)

emanoteModelDynamic :: (MonadUnliftIO m, MonadLoggerIO m) => Some Ema.CLI.Action -> RouteEncoder Model.Model SiteRoute -> CLI.Cli -> m (Dynamic m Model.Model)
emanoteModelDynamic cliAct enc cli = do
  defaultLayer <- Loc.defaultLayer <$> liftIO Paths_emanote.getDataDir
  instanceId <- liftIO UUID.nextRandom
  let layers = one defaultLayer <> Loc.userLayers (CLI.layers cli)
  emanate
    layers
    Pattern.filePatterns
    Pattern.ignorePatterns
    (Model.emptyModel layers cliAct enc instanceId)
    Patch.patchModel

type ChangeHandler tag model m = tag -> FilePath -> UM.FileAction (NonEmpty (Loc, FilePath)) -> m (model -> model)

-- | Emanate on-disk sources onto an in-memory `model` (stored in a LVar)
--
-- This is a generic extension to unionMountOnLVar for operating Emanote like
-- apps.
emanate ::
  forall m tag model.
  ( MonadLogger m,
    MonadUnliftIO m,
    Ord tag,
    model ~ Model.Model,
    tag ~ FileType SourceExt
  ) =>
  -- Layers to mount
  Set Loc ->
  [(tag, FilePattern)] ->
  -- | Ignore patterns
  [FilePattern] ->
  model ->
  ChangeHandler tag model m ->
  m (Dynamic m model)
emanate layers filePatterns ignorePatterns initialModel f = do
  Dynamic
    <$> UM.unionMount
      (layers & Set.map (\layer -> (layer, Loc.locPath layer)))
      filePatterns
      ignorePatterns
      initialModel
      (mapFsChanges f)

mapFsChanges :: (MonadIO m, MonadLogger m) => ChangeHandler tag model m -> UM.Change Loc tag -> m (model -> model)
mapFsChanges h ch = do
  withBlockBuffering $
    uncurry (mapFsChangesOnExt h) `chainM` Map.toList ch
  where
    -- Temporarily use block buffering before calling an IO action that is
    -- known ahead to log rapidly, so as to not hamper serial processing speed.
    withBlockBuffering f =
      hSetBuffering stdout (BlockBuffering Nothing)
        *> f
        <* (hSetBuffering stdout LineBuffering >> hFlush stdout)

mapFsChangesOnExt ::
  (MonadIO m, MonadLogger m) =>
  ChangeHandler tag model m ->
  tag ->
  Map FilePath (UM.FileAction (NonEmpty (Loc, FilePath))) ->
  m (model -> model)
mapFsChangesOnExt h fpType fps = do
  uncurry (h fpType) `chainM` Map.toList fps
