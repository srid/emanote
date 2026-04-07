module Emanote.Model.QuerySpec where

import Data.TagTree (Tag (Tag))
import Emanote.Model.Query
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Relude
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = do
  describe "tags" $ do
    it "tags can begin with hash" . hedgehog $ do
      s <- forAll $ Gen.text (Range.linear 2 20) Gen.alphaNum
      let q = "tag:#" <> s
          tag = Tag s
      parseQuery q === Just (QueryByTag tag)
