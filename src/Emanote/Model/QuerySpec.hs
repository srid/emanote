module Emanote.Model.QuerySpec where

import Data.TagTree (Tag (Tag))
import Emanote.Model.Query
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Relude
import Test.Tasty
import Test.Tasty.Hedgehog

spec :: TestTree
spec =
  testGroup
    "Tag query"
    [ testProperty "tags with #" prop_tagsCanBeginWithHash
    ]

prop_tagsCanBeginWithHash :: Property
prop_tagsCanBeginWithHash =
  property $ do
    s <- forAll $ Gen.text (Range.linear 2 20) Gen.alphaNum
    let q = "tag:#" <> s
        tag = Tag s
    parseQuery q === Just (QueryByTag tag)
    pure ()
