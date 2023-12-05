module Emanote.Pandoc.Renderer.CalloutSpec where

import Emanote.Pandoc.Renderer.Callout
import Hedgehog
import Relude
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = do
  describe "callout" $ do
    it "type" . hedgehog $ do
      parseCalloutType "[!tip]" === Just Tip
      parseCalloutType "[!note]" === Just Note
      parseCalloutType "[!info]" === Just Info
