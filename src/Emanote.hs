module Emanote
  ( emanate,
    ChangeHandler,
  )
where

import Control.Monad.Logger (MonadLogger)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.UUID.V4 qualified as UUID
import Ema
import Ema.Route.Encoder (encodeRoute)
import Emanote.CLI qualified as CLI
import Emanote.Model.Type qualified as Model
import Emanote.Prelude (chainM)
import Emanote.Route.Ext (FileType, SourceExt)
import Emanote.Route.SiteRoute.Class (indexRoute)
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

instance HasAsset SiteRoute where
  routeAsset enc m r = do
    View.render m r
      <&> fixStaticUrl
    where
      -- See the FIXME in more-head.tpl. This is a workaround for that.
      fixStaticUrl s =
        case findPrefix of
          Nothing -> s
          Just prefix ->
            encodeUtf8 . T.replace "(_emanote-static/" ("(" <> prefix <> "_emanote-static/") . decodeUtf8 $ s
        where
          findPrefix :: Maybe Text
          findPrefix = do
            let indexR = toText $ encodeRoute enc m $ indexRoute
            prefix <- T.stripSuffix "-/all.html" indexR
            guard $ not $ T.null prefix
            pure prefix

instance HasModel SiteRoute where
  type ModelInput SiteRoute = CLI.Cli
  modelDynamic cliAct enc cli = do
    defaultLayer <- Loc.defaultLayer <$> liftIO Paths_emanote.getDataDir
    instanceId <- liftIO UUID.nextRandom
    let layers = one defaultLayer <> Loc.userLayers (CLI.layers cli)
    Emanote.emanate
      layers
      Pattern.filePatterns
      Pattern.ignorePatterns
      (Model.emptyModel cliAct enc instanceId)
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
  Set (Loc, FilePath) ->
  [(tag, FilePattern)] ->
  -- | Ignore patterns
  [FilePattern] ->
  model ->
  ChangeHandler tag model m ->
  m (Dynamic m model)
emanate layers filePatterns ignorePatterns initialModel f = do
  Dynamic
    <$> UM.unionMount1
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
