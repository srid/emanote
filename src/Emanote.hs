module Emanote
  ( mkEmanoteSite,
    emanate,
    ChangeHandler,
  )
where

import Control.Monad.Logger (MonadLogger)
import Data.LVar (LVar)
import Data.Map.Strict qualified as Map
import Data.UUID.V4 qualified as UUID
import Ema
import Emanote.CLI qualified as CLI
import Emanote.Model.Type qualified as Model
import Emanote.Prelude (chainM)
import Emanote.Route.SiteRoute.Class (routeEncoder)
import Emanote.Route.SiteRoute.Type (SiteRoute)
import Emanote.Source.Loc (Loc)
import Emanote.Source.Loc qualified as Loc
import Emanote.Source.Patch qualified as Patch
import Emanote.Source.Pattern qualified as Pattern
import Emanote.View qualified as View
import Paths_emanote qualified
import Relude
import System.FilePattern (FilePattern)
import System.UnionMount qualified as UM
import UnliftIO (BufferMode (..), MonadUnliftIO, hSetBuffering)
import UnliftIO.IO (hFlush)

-- | Create an Emanote Ema site.
mkEmanoteSite :: CLI.Cli -> Site Model.Model SiteRoute
mkEmanoteSite cli =
  Site
    { siteName = "emanote",
      siteRender = render,
      siteRouteEncoder = routeEncoder,
      siteModelManager = modelManager cli
    }

render :: SiteRender Model.Model SiteRoute
render =
  SiteRender $ \m r -> do
    pure $ View.render m r

modelManager :: CLI.Cli -> ModelManager Model.Model SiteRoute
modelManager cli = ModelManager $ do
  cliAct <- askCLIAction
  enc <- askRouteEncoder
  defaultLayer <- Loc.defaultLayer <$> liftIO Paths_emanote.getDataDir
  instanceId <- liftIO UUID.nextRandom
  let layers = one defaultLayer <> Loc.userLayers (CLI.layers cli)
  lift $
    Emanote.emanate
      layers
      Pattern.filePatterns
      Pattern.ignorePatterns
      (Model.emptyModel cliAct enc instanceId)
      Patch.patchModel

type ChangeHandler tag model m = tag -> FilePath -> UM.FileAction (NonEmpty (Loc, FilePath)) -> m (model -> model)

-- TODO: import from ... unionmount?
type NonEmptyLVar m a =
  ( -- Initial value
    a,
    -- Generator for subsequent values
    LVar a -> m ()
  )

-- | Emanate on-disk sources onto an in-memory `model` (stored in a LVar)
--
-- This is a generic extension to unionMountOnLVar for operating Emanote like
-- apps.
emanate ::
  (MonadUnliftIO m, MonadLogger m, Ord tag) =>
  -- Layers to mount
  Set (Loc, FilePath) ->
  [(tag, FilePattern)] ->
  -- | Ignore patterns
  [FilePattern] ->
  model ->
  ChangeHandler tag model m ->
  m (NonEmptyLVar m model)
emanate layers filePatterns ignorePatterns initialModel f = do
  UM.unionMountOnLVar
    layers
    filePatterns
    ignorePatterns
    initialModel
    (mapFsChanges f)

{- TODO: support this:
log "!! Remounting !!"
LVar.set modelLvar initialModel -- Reset the model
emanate layers filePatterns ignorePatterns modelLvar initialModel f
-}

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
