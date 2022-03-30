{-# LANGUAGE TemplateHaskell #-}

module Emanote.Model.Link.RelSpec where

import Emanote.Model.Link.Rel
import Hedgehog
import Relude
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = do
  describe "dropDotDot" $ do
    it "simple" . hedgehog $ do
      dropDotDot "foo/bar/qux" === "foo/bar/qux"
      dropDotDot "foo/../qux" === "qux"
      dropDotDot "bar/foo/../qux" === "bar/qux"
      dropDotDot "bar/foo/.." === "bar"
    it "dotInfix" . hedgehog $ do
      dropDotDot "bar/foo/../../qux" === "qux"
      dropDotDot "bar/foo/../../qux/../foo" === "foo"
    it "dotPrefix" . hedgehog $ do
      dropDotDot "../../foo" === "foo"
      dropDotDot "../foo" === "foo"
      dropDotDot "./../foo" === "foo"
      dropDotDot "./foo" === "./foo"
    it "dotSuffix" . hedgehog $ do
      dropDotDot "foo/.." === ""
      dropDotDot "foo/bar/.." === "foo"
      dropDotDot "foo/bar/../.." === ""

-- https://github.com/hedgehogqa/haskell-hedgehog/issues/121
once :: PropertyT IO () -> Property
once = withTests 1 . property
