{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Emanote.Source.Dynamic
  ( emanoteSiteInput,
    EmanoteConfig (..),
    emanoteCompileTailwind,
    emanoteConfigCli,
    emanoteConfigNoteFn,
    emanoteConfigPandocRenderers,
  )
where

import Control.Monad.Logger (MonadLogger, MonadLoggerIO)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Some (Some)
import Data.UUID.V4 qualified as UUID
import Ema (Dynamic (..))
import Ema.CLI qualified
import Emanote.CLI qualified as CLI
import Emanote.Model.Note (Note)
import Emanote.Model.Type qualified as Model
import Emanote.Pandoc.Renderer (EmanotePandocRenderers)
import Emanote.Prelude (chainM)
import Emanote.Route (LMLRoute)
import Emanote.Source.Loc (Loc)
import Emanote.Source.Loc qualified as Loc
import Emanote.Source.Patch qualified as Patch
import Emanote.Source.Pattern qualified as Pattern
import Optics.TH (makeLenses)
import Paths_emanote qualified
import Relude
import System.UnionMount qualified as UM
import UnliftIO (MonadUnliftIO)

-- | Everything that's required to run an Emanote site.
data EmanoteConfig = EmanoteConfig
  { -- | CLI arguments (includes layers)
    _emanoteConfigCli :: CLI.Cli,
    -- | A function to filter the `Note` before it gets added to the model.
    _emanoteConfigNoteFn :: Note -> Note,
    -- | How to render Pandoc to Heist HTML.
    _emanoteConfigPandocRenderers :: EmanotePandocRenderers Model.Model LMLRoute,
    -- | Whether to replace Tailwind2 CDN with a minimized Tailwind3 CSS file.
    _emanoteCompileTailwind :: Bool
  }

-- | Make an Ema `Dynamic` for the Emanote model.
--
-- The bulk of logic for building the Dynamic is in `Patch.hs`.
emanoteSiteInput :: (MonadUnliftIO m, MonadLoggerIO m) => Some Ema.CLI.Action -> EmanoteConfig -> m (Dynamic m Model.ModelEma)
emanoteSiteInput cliAct EmanoteConfig {..} = do
  defaultLayer <- Loc.defaultLayer <$> liftIO Paths_emanote.getDataDir
  instanceId <- liftIO UUID.nextRandom
  let layers = Loc.userLayers (CLI.layers _emanoteConfigCli) <> one defaultLayer
      initialModel = Model.emptyModel layers cliAct _emanoteConfigPandocRenderers _emanoteCompileTailwind instanceId
  Dynamic
    <$> UM.unionMount
      (layers & Set.map (id &&& Loc.locPath))
      Pattern.filePatterns
      Pattern.ignorePatterns
      initialModel
      (mapFsChanges $ Patch.patchModel layers _emanoteConfigNoteFn)

type ChangeHandler tag model m =
  tag ->
  FilePath ->
  UM.FileAction (NonEmpty (Loc, FilePath)) ->
  m (model -> model)

mapFsChanges :: (MonadIO m, MonadLogger m) => ChangeHandler tag model m -> UM.Change Loc tag -> m (model -> model)
mapFsChanges h ch = do
  uncurry (mapFsChangesOnExt h) `chainM` Map.toList ch
  where
    -- Temporarily use block buffering before calling an IO action that is
    -- known ahead to log rapidly, so as to not hamper serial processing speed.
    -- FIXME: This buffers warnings and errors (when parsing .md file) without
    -- dumping them to console. So disabling for now. But we need a proper fix.
    _withBlockBuffering f =
      (hSetBuffering stdout (BlockBuffering Nothing) >> hSetBuffering stderr LineBuffering)
        *> f
        <* (hFlush stdout >> hFlush stderr >> hSetBuffering stdout LineBuffering)

mapFsChangesOnExt ::
  (MonadIO m, MonadLogger m) =>
  ChangeHandler tag model m ->
  tag ->
  Map FilePath (UM.FileAction (NonEmpty (Loc, FilePath))) ->
  m (model -> model)
mapFsChangesOnExt h fpType fps = do
  uncurry (h fpType) `chainM` Map.toList fps

makeLenses ''EmanoteConfig
