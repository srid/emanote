module Main where

import Control.Monad.Logger (runStdoutLoggingT)
import Data.Default (Default (def))
import Data.Dependent.Sum (DSum ((:=>)))
import Ema
import Ema.CLI qualified
import Emanote ()
import Emanote.CLI qualified as CLI
import Emanote.Route.SiteRoute (SiteRoute)
import Emanote.View.Common (generatedCssFile)
import Main.Utf8 (withUtf8)
import Optics.Core ((%), (.~))
import Relude
import Spec qualified
import System.Environment qualified as Env
import System.FilePath ((</>))
import Web.Tailwind qualified as Tailwind

main :: IO ()
main =
  withUtf8 $ do
    cli <- CLI.parseCli
    if CLI.test cli
      then test
      else run cli

test :: IO ()
test = do
  Env.withArgs
    mempty --Discard emanote's arguments
    Spec.main

run :: CLI.Cli -> IO ()
run cli = do
  let emaCli = CLI.emaCli cli
  ret <-
    Ema.runSiteWithCli @SiteRoute emaCli cli
  case ret of
    Ema.CLI.Generate outPath :=> Identity genPaths -> do
      let cssPath = outPath </> generatedCssFile
      putStrLn $ "Compiling CSS using tailwindcss: " <> cssPath
      runStdoutLoggingT . Tailwind.runTailwind $
        def
          & Tailwind.tailwindConfig % Tailwind.tailwindConfigContent .~ genPaths
          & Tailwind.tailwindOutput .~ cssPath
          & Tailwind.tailwindMode .~ Tailwind.Production
    _ -> do
      pure ()
