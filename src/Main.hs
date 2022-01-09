{-# LANGUAGE GADTs #-}

module Main where

import Control.Lens.Operators
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Default (Default (def))
import Data.Dependent.Sum (DSum ((:=>)))
import qualified Ema
import qualified Ema.CLI
import qualified Emanote
import qualified Emanote.CLI as CLI
import qualified Emanote.Model as Model
import qualified Emanote.Source.Loc as Loc
import qualified Emanote.Source.Patch as Patch
import qualified Emanote.Source.Pattern as Pattern
import qualified Emanote.View as View
import Emanote.View.Common (generatedCssFile)
import Main.Utf8 (withUtf8)
import qualified Paths_emanote
import Relude
import qualified Spec
import qualified System.Environment as Env
import System.FilePath ((</>))
import qualified Web.Tailwind as Tailwind

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
  res <-
    Ema.runEmaWithCli (CLI.emaCli cli) (const View.render) $ \act m -> do
      defaultLayer <- Loc.defaultLayer <$> liftIO Paths_emanote.getDataDir
      let layers = one defaultLayer <> Loc.userLayers (CLI.layers cli)
      Emanote.emanate
        layers
        Pattern.filePatterns
        Pattern.ignorePatterns
        m
        (Model.emptyModel act)
        Patch.patchModel
  case res of
    Right (Ema.CLI.Generate outPath :=> Identity genPaths) -> do
      let cssPath = outPath </> generatedCssFile
      putStrLn $ "Compiling CSS using tailwindcss: " <> cssPath
      runStdoutLoggingT . Tailwind.runTailwind $
        def
          & Tailwind.tailwindConfig . Tailwind.tailwindConfigContent .~ genPaths
          & Tailwind.tailwindOutput .~ cssPath
          & Tailwind.tailwindMode .~ Tailwind.Production
    _ ->
      pure ()
