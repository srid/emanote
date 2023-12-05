module Emanote.Pandoc.Renderer.CalloutSpec where

import Hedgehog
import Relude
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = do
  describe "callout" $ do
    it "type" . hedgehog $ do
      True === True
