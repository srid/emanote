{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Lens.Operators ((.~))
import Control.Monad.Logger (MonadLogger)
import Data.Default (Default (def))
import Data.LVar (LVar)
import qualified Ema
import qualified Ema.CLI as CLI
import qualified Ema.Helper.Tailwind as Tailwind
import Emanote.Class ()
import Emanote.Model (Model)
import qualified Emanote.Model as M
import qualified Emanote.Source as Source
import qualified Emanote.Source.Mount as Mount
import qualified Emanote.Template as Template
import qualified Heist.Extra.TemplateState as T
import Main.Utf8 (withUtf8)
import UnliftIO (BufferMode (BlockBuffering), MonadUnliftIO, hSetBuffering)

main :: IO ()
main = do
  liftIO $ print =<< hSetBuffering stdout (BlockBuffering Nothing)
  withUtf8 $
    Ema.runEma (Template.render . cssShim) run
  where
    cssShim =
      Tailwind.twindShim

run :: (MonadUnliftIO m, MonadLogger m) => CLI.Action -> LVar Model -> m ()
run _act modelLvar = do
  -- TODO: When CLI.Action is Generate, exit immediately instead of monitoring mounts.
  fsLayers <- liftIO Source.locLayers
  emptyTmpl <- T.newTemplateState
  let initialModel = def & M.modelHeistTemplate .~ emptyTmpl
  Mount.unionMountOnLVar
    fsLayers
    Source.filePatterns
    Source.ignorePatterns
    modelLvar
    initialModel
    Source.transformActions
