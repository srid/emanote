{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Ema
import qualified Emanote
import qualified Emanote.CLI as CLI
import qualified Emanote.Model as Model
import qualified Emanote.Source.Loc as Loc
import qualified Emanote.Source.Patch as Patch
import qualified Emanote.Source.Pattern as Pattern
import qualified Emanote.View as View
import Main.Utf8 (withUtf8)
import qualified Paths_emanote
import Relude
import qualified Spec
import qualified System.Environment as Env

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
  void $
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
