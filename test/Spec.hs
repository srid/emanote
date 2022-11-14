module Spec (main) where

import Emanote.Model.Link.RelSpec qualified as RelSpec
import Emanote.Model.QuerySpec qualified as QuerySpec
import Emanote.Pandoc.BuiltinFiltersSpec qualified as BuiltinFiltersSpec
import Relude
import Test.Hspec (hspec)

main :: IO ()
main = do
  hspec $ do
    QuerySpec.spec
    RelSpec.spec
    BuiltinFiltersSpec.spec
