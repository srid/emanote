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
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.UUID.V4 qualified as UUID
import Ema (Dynamic (..))
import Ema.CLI qualified
import Emanote.CLI qualified as CLI
import Emanote.Model.Note (Note)
import Emanote.Model.Stork.Index qualified as Stork
import Emanote.Model.Type qualified as Model
import Emanote.Pandoc.Renderer (EmanotePandocRenderers)
import Emanote.Route (LMLRoute)
import Emanote.Route qualified as R
import Emanote.Source.Ignore qualified as Ignore
import Emanote.Source.Loc (Loc)
import Emanote.Source.Loc qualified as Loc
import Emanote.Source.Patch qualified as Patch
import Emanote.Source.Pattern qualified as Pattern
import Optics.TH (makeLenses)
import Paths_emanote qualified
import Relude
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

{- | Make an Ema `Dynamic` for the Emanote model. The bulk of logic
for building the Dynamic is in @Source/Patch.hs@.

We use 'UM.unionMountStreaming' so per-file closures are GC'd as
their updates land (caps initial-load memory on large notebooks —
@srid/emanote#66@) and so 'Patch.patchModel' can read the running
model when walking the Lua-filter dep index (@srid/emanote#263@).
-}
emanoteSiteInput :: (MonadUnliftIO m, MonadLoggerIO m) => Ema.CLI.Action -> EmanoteConfig -> m (Dynamic m Model.ModelEma)
emanoteSiteInput cliAct EmanoteConfig {..} = do
  defaultLayer <- Loc.defaultLayer <$> liftIO Paths_emanote.getDataDir
  instanceId <- liftIO UUID.nextRandom
  storkIndex <- Stork.newIndex
  let layers = Loc.userLayers ((CLI.path &&& CLI.mountPoint) <$> CLI.layers _emanoteConfigCli) <> one defaultLayer
      initialModel = Model.emptyModel layers cliAct _emanoteConfigPandocRenderers _emanoteCompileTailwind instanceId storkIndex
  scriptingEngine <- getEngine
  -- NOTE: Per-layer ignore patterns are loaded once at startup. Edits
  -- to a `.emanoteignore` file during a live-serve session do not take
  -- effect until restart. Live reload (#228 phase 2) requires
  -- threading an IO refresh through unionmount's recursive remount
  -- path, which is currently a static `Map source [FilePattern]`.
  perLayerIgnore <- Ignore.loadIgnorePatterns layers
  let
    -- unionmount has only one ignore input (per-source); fold the
    -- universal patterns into every layer's entry so they apply
    -- regardless of whether the layer has its own `.emanoteignore`.
    universalForEveryLayer = Map.fromSet (const Pattern.ignorePatterns) layers
    ignoreByLayer = Map.unionWith (<>) universalForEveryLayer perLayerIgnore
    handle :: (MonadUnliftIO m, MonadLoggerIO m) => UM.Change Loc (R.FileType R.SourceExt) -> Model.ModelEma -> m Model.ModelEma
    handle change m0 =
      foldlM applyOne m0 (changeEntries change)
    applyOne :: (MonadUnliftIO m, MonadLoggerIO m) => Model.ModelEma -> (R.FileType R.SourceExt, FilePath, UM.FileAction (NonEmpty (Loc, FilePath))) -> m Model.ModelEma
    applyOne m (fpType, fp, action) = do
      trans <- Patch.patchModel layers _emanoteConfigNoteFn storkIndex scriptingEngine m fpType fp action
      pure $! trans m
  Dynamic
    <$> UM.unionMountStreaming
      (layers & Set.map (id &&& Loc.locPath))
      Pattern.filePatterns
      ignoreByLayer
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

makeLenses ''EmanoteConfig
