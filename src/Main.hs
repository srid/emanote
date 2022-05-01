module Main where

import Emanote (defaultEmanoteConfig, run)
import Emanote.CLI qualified as CLI
import Main.Utf8 (withUtf8)
import Relude
import Spec qualified
import System.Environment qualified as Env

main :: IO ()
main =
  withUtf8 $ do
    cli <- CLI.parseCli
    if CLI.test cli
      then test
      else run (defaultEmanoteConfig cli)

test :: IO ()
test = do
  Env.withArgs
    mempty --Discard emanote's CLI arguments
    Spec.main
