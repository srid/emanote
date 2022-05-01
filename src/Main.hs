module Main where

import Emanote
import Emanote.CLI qualified as CLI
import Emanote.Source.Dynamic (EmanoteConfig (EmanoteConfig))
import Emanote.View.Common (defaultEmanotePandocRenderers)
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
      else run $ EmanoteConfig cli id defaultEmanotePandocRenderers

test :: IO ()
test = do
  Env.withArgs
    mempty --Discard emanote's CLI arguments
    Spec.main
