module Main where

import Emanote.Route.RSpec qualified
import Relude
import Test.Hspec

main :: IO ()
main =
  hspec Emanote.Route.RSpec.spec
