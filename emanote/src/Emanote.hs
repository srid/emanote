{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Emanote (
  run,
  defaultEmanoteConfig,
) where

import Control.Monad.Logger (LogLevel (LevelError), runStderrLoggingT, runStdoutLoggingT)
import Control.Monad.Logger.Extras (Logger (Logger), logToStderr, runLoggerLoggingT)
import Control.Monad.Writer.Strict (MonadWriter (tell), WriterT (runWriterT))
import Data.Default (def)
import Data.Map.Strict qualified as Map
import Ema (
  EmaSite (..),
  IsRoute (..),
  SiteConfig (SiteConfig),
  fromPrism_,
  runSiteWith,
  toPrism_,
 )
import Ema.CLI qualified
import Ema.Dynamic (Dynamic (Dynamic))
import Emanote.CLI qualified as CLI
import Emanote.Model.Graph qualified as G
import Emanote.Model.Link.Rel (ResolvedRelTarget (..))
import Emanote.Model.Type (modelCompileTailwind)
import Emanote.Model.Type qualified as Model
import Emanote.Pandoc.Renderer
import Emanote.Pandoc.Renderer.Callout qualified as PF
import Emanote.Pandoc.Renderer.Embed qualified as PF
import Emanote.Pandoc.Renderer.Query qualified as PF
import Emanote.Pandoc.Renderer.Url qualified as PF
import Emanote.Prelude (log, logE, logW)
import Emanote.Route.ModelRoute (LMLRoute, lmlRouteCase)
import Emanote.Route.SiteRoute.Class (emanoteGeneratableRoutes, emanoteRouteEncoder)
import Emanote.Route.SiteRoute.Type (SiteRoute)
import Emanote.Source.Dynamic (EmanoteConfig (..), emanoteSiteInput)
import Emanote.View.Common (generatedCssFile)
import Emanote.View.Export qualified as Export
import Emanote.View.Template qualified as View
import Optics.Core ((%), (.~), (^.))
import Relude
import System.FilePath ((</>))
import UnliftIO (MonadUnliftIO)
import Web.Tailwind qualified as Tailwind

instance IsRoute SiteRoute where
  type RouteModel SiteRoute = Model.ModelEma
  routePrism = toPrism_ . emanoteRouteEncoder
  routeUniverse = emanoteGeneratableRoutes

instance EmaSite SiteRoute where
  type SiteArg SiteRoute = EmanoteConfig
  siteInput cliAct cfg = do
    model <- emanoteSiteInput cliAct cfg
    pure $ model <&> modelUpdateCachedFields
  siteOutput = View.emanoteSiteOutput

-- | Populate model fields that needs to be computed once per update.
modelUpdateCachedFields :: Model.ModelEma -> Model.ModelEma
modelUpdateCachedFields model =
  model
    & Model.modelFolgezettelTree
    .~ G.folgezettelTreesFrom (unModelEma model) (Model.modelIndexRoute model)

defaultEmanoteConfig :: CLI.Cli -> EmanoteConfig
defaultEmanoteConfig cli =
  EmanoteConfig cli id defaultEmanotePandocRenderers False

run :: EmanoteConfig -> IO ()
run cfg@EmanoteConfig {..} = do
  case CLI.cmd _emanoteConfigCli of
    CLI.Cmd_Ema emaCli -> do
      let emaCfg = SiteConfig emaCli def
      Ema.runSiteWith @SiteRoute emaCfg cfg
        >>= postRun cfg
    CLI.Cmd_Export exportFormat -> do
      Dynamic (unModelEma -> model0, _) <-
        flip runLoggerLoggingT oneOffLogger
          $ siteInput @SiteRoute (Ema.CLI.action def) cfg
      content <- Export.renderExport exportFormat model0
      putLBSLn content
  where
    -- A logger suited for running one-off commands.
    oneOffLogger =
      logToStderr
        & allowLogLevelFrom LevelError
      where
        allowLogLevelFrom :: LogLevel -> Logger -> Logger
        allowLogLevelFrom minLevel (Logger f) = Logger $ \loc src level msg ->
          if level >= minLevel
            then f loc src level msg
            else pass

postRun :: EmanoteConfig -> (Model.ModelEma, (FilePath, [FilePath])) -> IO ()
postRun EmanoteConfig {..} (unModelEma -> model0, (outPath, genPaths)) = do
  when (model0 ^. modelCompileTailwind)
    $ compileTailwindCss (outPath </> generatedCssFile) genPaths
  checkBrokenLinks _emanoteConfigCli $ Export.modelRels model0
  checkBadMarkdownFiles $ Model.modelNoteErrors model0

unModelEma :: Model.ModelEma -> Model.Model
unModelEma m = Model.withRoutePrism (fromPrism_ $ routePrism @SiteRoute m) m

checkBadMarkdownFiles :: Map LMLRoute [Text] -> IO ()
checkBadMarkdownFiles noteErrs = runStderrLoggingT $ do
  forM_ (Map.toList noteErrs) $ \(noteRoute, errs) -> do
    logW $ "Bad markdown file: " <> show noteRoute
    forM_ errs $ \err -> do
      logE $ "  - " <> err
  unless (null noteErrs) $ do
    logE "Errors found."
    exitFailure

checkBrokenLinks :: CLI.Cli -> Map LMLRoute [Export.Link] -> IO ()
checkBrokenLinks cli modelRels = runStderrLoggingT $ do
  ((), res :: Sum Int) <- runWriterT
    $ forM_ (Map.toList modelRels)
    $ \(noteRoute, rels) ->
      forM_ (sortNub rels) $ \(Export.Link urt rrt) ->
        case rrt of
          RRTFound _ -> pass
          RRTMissing -> do
            logW $ "Broken link: " <> show (lmlRouteCase noteRoute) <> " -> " <> show urt
            tell 1
          RRTAmbiguous ls -> do
            logW $ "Ambiguous link: " <> show (lmlRouteCase noteRoute) <> " -> " <> show urt <> " ambiguities: " <> show ls
            tell 1
  if res == 0
    then do
      log "No broken links detected."
    else unless (CLI.allowBrokenLinks cli) $ do
      logE $ "Found " <> show (getSum res) <> " broken links! Emanote generated the site, but the generated site has broken links."
      log "(Tip: use `--allow-broken-links` to ignore this check.)"
      exitFailure

compileTailwindCss :: (MonadUnliftIO m) => FilePath -> [FilePath] -> m ()
compileTailwindCss cssPath genPaths = do
  runStdoutLoggingT $ do
    log $ "Running Tailwind CSS v3 compiler to generate: " <> toText cssPath
    Tailwind.runTailwind
      $ def
      & Tailwind.tailwindConfig
      % Tailwind.tailwindConfigContent
      .~ genPaths
      & Tailwind.tailwindOutput
      .~ cssPath
      & Tailwind.tailwindMode
      .~ Tailwind.Production

defaultEmanotePandocRenderers :: EmanotePandocRenderers Model.Model LMLRoute
defaultEmanotePandocRenderers =
  let blockRenderers =
        PandocRenderers
          [ PF.embedInlineWikiLinkResolvingSplice -- embedInlineWikiLinkResolvingSplice should be first to recognize inline Link elements first
          , PF.urlResolvingSplice
          ]
          [ PF.embedBlockWikiLinkResolvingSplice
          , PF.embedBlockRegularLinkResolvingSplice
          , PF.queryResolvingSplice
          , PF.calloutResolvingSplice
          ]
      inlineRenderers =
        PandocRenderers
          [ PF.embedInlineWikiLinkResolvingSplice -- embedInlineWikiLinkResolvingSplice should be first to recognize inline Link elements first
          , PF.urlResolvingSplice
          ]
          mempty
      linkInlineRenderers =
        PandocRenderers
          [ PF.plainifyWikiLinkSplice
          ]
          mempty
   in EmanotePandocRenderers {..}
