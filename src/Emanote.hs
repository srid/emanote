{-# OPTIONS_GHC -Wno-orphans #-}

module Emanote (run) where

import Control.Monad.Logger (runStdoutLoggingT)
import Data.Default (def)
import Data.Dependent.Sum
import Ema
import Ema.CLI qualified
import Emanote.CLI qualified as CLI
import Emanote.Model.Type qualified as Model
import Emanote.Route.SiteRoute.Class (emanoteGeneratableRoutes, emanoteRouteEncoder)
import Emanote.Route.SiteRoute.Type (SiteRoute)
import Emanote.Source.Dynamic (emanoteModelDynamic)
import Emanote.View.Common (generatedCssFile)
import Emanote.View.Template qualified as View
import Optics.Core ((%), (.~))
import Relude
import System.FilePath ((</>))
import UnliftIO (MonadUnliftIO)
import Web.Tailwind qualified as Tailwind

instance IsRoute SiteRoute where
  type RouteModel SiteRoute = Model.Model
  routeEncoder = emanoteRouteEncoder

instance CanGenerate SiteRoute where
  generatableRoutes = emanoteGeneratableRoutes

instance CanRender SiteRoute where
  routeAsset = View.emanoteRouteAsset

instance HasModel SiteRoute where
  type ModelInput SiteRoute = CLI.Cli
  modelDynamic = emanoteModelDynamic

run :: CLI.Cli -> IO ()
run cli = do
  Ema.runSiteWithCli @SiteRoute (CLI.emaCli cli) cli >>= \case
    Ema.CLI.Generate outPath :=> Identity genPaths ->
      compileTailwindCss outPath genPaths
    _ ->
      pure ()

compileTailwindCss :: MonadUnliftIO m => FilePath -> [FilePath] -> m ()
compileTailwindCss outPath genPaths = do
  let cssPath = outPath </> generatedCssFile
  putStrLn $ "Compiling CSS using tailwindcss: " <> cssPath
  runStdoutLoggingT . Tailwind.runTailwind $
    def
      & Tailwind.tailwindConfig % Tailwind.tailwindConfigContent .~ genPaths
      & Tailwind.tailwindOutput .~ cssPath
      & Tailwind.tailwindMode .~ Tailwind.Production
