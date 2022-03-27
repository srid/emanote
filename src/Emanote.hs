{-# OPTIONS_GHC -Wno-orphans #-}

module Emanote
  ( run,
  )
where

import Control.Monad.Logger (runStderrLoggingT, runStdoutLoggingT)
import Control.Monad.Writer.Strict (MonadWriter (tell), WriterT (runWriterT))
import Data.Default (def)
import Data.Dependent.Sum (DSum ((:=>)))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Ema
  ( CanGenerate (..),
    CanRender (..),
    HasModel (..),
    IsRoute (..),
    runSiteWithCli,
  )
import Ema.CLI qualified
import Emanote.CLI qualified as CLI
import Emanote.Model.Link.Rel (ResolvedRelTarget (..))
import Emanote.Model.Type qualified as Model
import Emanote.Prelude (log, logE, logW)
import Emanote.Route.ModelRoute (lmlRouteCase)
import Emanote.Route.SiteRoute.Class (emanoteGeneratableRoutes, emanoteRouteEncoder)
import Emanote.Route.SiteRoute.Type (SiteRoute)
import Emanote.Source.Dynamic (emanoteModelDynamic)
import Emanote.View.Common (generatedCssFile)
import Emanote.View.Export qualified as Export
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
    (model0, Ema.CLI.Generate outPath :=> Identity genPaths) -> do
      compileTailwindCss outPath genPaths
      checkBrokenLinks cli model0
    _ ->
      pure ()

checkBrokenLinks :: CLI.Cli -> Model.Model -> IO ()
checkBrokenLinks cli model = runStderrLoggingT $ do
  ((), res :: Sum Int) <- runWriterT $
    forM_ (Map.toList $ Export.modelRels model) $ \(noteRoute, rels) ->
      forM_ (Set.toList $ Set.fromList rels) $ \(Export.Link urt rrt) ->
        case rrt of
          RRTFound _ -> pure ()
          RRTMissing -> do
            logW $ "Broken link: " <> show (lmlRouteCase noteRoute) <> " -> " <> show urt
            tell 1
          RRTAmbiguous ls -> do
            logW $ "Ambiguous link: " <> show (lmlRouteCase noteRoute) <> " -> " <> show urt <> " ambiguities: " <> show ls
            tell 1
  unless (res == 0 || CLI.allowBrokenLinks cli) $ do
    logE $ "Found " <> show (getSum res) <> " broken links! Emanote generated the site, but the generated site has broken links."
    log "(Tip: use `--allow-broken-links` to ignore this check.)"
    exitFailure

compileTailwindCss :: MonadUnliftIO m => FilePath -> [FilePath] -> m ()
compileTailwindCss outPath genPaths = do
  let cssPath = outPath </> generatedCssFile
  putStrLn $ "Compiling CSS using tailwindcss: " <> cssPath
  runStdoutLoggingT . Tailwind.runTailwind $
    def
      & Tailwind.tailwindConfig % Tailwind.tailwindConfigContent .~ genPaths
      & Tailwind.tailwindOutput .~ cssPath
      & Tailwind.tailwindMode .~ Tailwind.Production
