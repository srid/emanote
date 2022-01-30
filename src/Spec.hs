module Spec (main) where

import Emanote.Model.QuerySpec qualified as QuerySpec
import Relude
import Test.Tasty
import Test.Tasty.Hedgehog
  ( HedgehogShrinkLimit (..),
    HedgehogTestLimit (..),
  )

main :: IO ()
main = do
  defaultMain tests

-- | Number of successful tests for each hedgehog property.
limit :: HedgehogTestLimit
limit = HedgehogTestLimit (Just 10)

tests :: TestTree
tests =
  localOption limit . localOption (HedgehogShrinkLimit (Just 2)) $
    testGroup
      "Tests"
      [ QuerySpec.spec
      ]
