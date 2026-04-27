module Main where

import Emanote.Pandoc.ExternalLinkSpec qualified
import Relude
import Test.Hspec

main :: IO ()
main =
  hspec Emanote.Pandoc.ExternalLinkSpec.spec
