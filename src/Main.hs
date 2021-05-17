{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Monad.Logger (MonadLogger)
import Data.LVar (LVar)
import Ema (Ema)
import qualified Ema
import qualified Ema.Helper.FileSystem as FileSystem
import Emabook.Model (Model)
import qualified Emabook.Model as M
import Emabook.Route (MarkdownRoute)
import qualified Emabook.Route as R
import qualified Emabook.Source as Source
import qualified Emabook.Template as Template
import UnliftIO (MonadUnliftIO)

instance Ema Model MarkdownRoute where
  encodeRoute = R.encodeRoute
  decodeRoute = R.decodeRoute
  staticRoutes = M.staticRoutes
  staticAssets _ =
    ["favicon.jpeg", "favicon.svg", "static"]

main :: IO ()
main =
  Ema.runEma (const Template.render) run

run :: (MonadUnliftIO m, MonadLogger m) => LVar Model -> m ()
run model =
  FileSystem.mountOnLVar "." Source.filePatterns model $ \sources action -> do
    Source.transformActions sources action
