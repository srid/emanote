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
    -- Why is test part of executable?
    -- Simply so that ghcid can reload both library and test at the same.
    -- Once Multiple Home Units for GHC is implemented, we can go back to
    -- using separate test stanza.
    if CLI.test cli
      then test
      else run (defaultEmanoteConfig cli)

test :: IO ()
test = do
  Env.withArgs
    mempty --Discard emanote's CLI arguments
    Spec.main
