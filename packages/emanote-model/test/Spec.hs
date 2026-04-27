module Main where

import Emanote.Model.Link.RelSpec qualified
import Emanote.Model.QuerySpec qualified
import Emanote.Model.TocSpec qualified
import Relude
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    Emanote.Model.Link.RelSpec.spec
    Emanote.Model.QuerySpec.spec
    Emanote.Model.TocSpec.spec
