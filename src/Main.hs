module Main where

import Control.Lens.Operators
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Default (Default (def))
import Data.Some
import Ema
import Ema.CLI qualified
import Emanote (mkEmanoteSite)
import Emanote.CLI qualified as CLI
import Emanote.View.Common (generatedCssFile)
import Main.Utf8 (withUtf8)
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
  genPaths <-
    Ema.runSiteWithCli emaCli $ mkEmanoteSite cli
  case Ema.CLI.action emaCli of
    Some (Ema.CLI.Generate outPath) -> do
      let cssPath = outPath </> generatedCssFile
      putStrLn $ "Compiling CSS using tailwindcss: " <> cssPath
      runStdoutLoggingT . Tailwind.runTailwind $
        def
          & Tailwind.tailwindConfig . Tailwind.tailwindConfigContent .~ genPaths
          & Tailwind.tailwindOutput .~ cssPath
          & Tailwind.tailwindMode .~ Tailwind.Production
    _ ->
      pure ()
