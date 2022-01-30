module Main where

import Control.Lens.Operators
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Default (Default (def))
import Data.Dependent.Sum (DSum ((:=>)))
import Data.UUID.V4 qualified as UUID
import Ema qualified
import Ema.CLI qualified
import Emanote qualified
import Emanote.CLI qualified as CLI
import Emanote.Model qualified as Model
import Emanote.Source.Loc qualified as Loc
import Emanote.Source.Patch qualified as Patch
import Emanote.Source.Pattern qualified as Pattern
import Emanote.View qualified as View
import Emanote.View.Common (generatedCssFile)
import Main.Utf8 (withUtf8)
import Paths_emanote qualified
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
  res <-
    Ema.runEmaWithCli (CLI.emaCli cli) (const View.render) $ \act m -> do
      defaultLayer <- Loc.defaultLayer <$> liftIO Paths_emanote.getDataDir
      instanceId <- liftIO UUID.nextRandom
      let layers = one defaultLayer <> Loc.userLayers (CLI.layers cli)
      Emanote.emanate
        layers
        Pattern.filePatterns
        Pattern.ignorePatterns
        m
        (Model.emptyModel act instanceId)
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
