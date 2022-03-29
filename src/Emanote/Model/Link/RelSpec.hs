{-# LANGUAGE TemplateHaskell #-}

module Emanote.Model.Link.RelSpec where

import Emanote.Model.Link.Rel
import Hedgehog
import Relude

tests :: IO Bool
tests =
  checkParallel $$(discover)

prop_dropDotDot_simple :: Property
prop_dropDotDot_simple = once $ do
  dropDotDot "foo/bar/qux" === "foo/bar/qux"
  dropDotDot "foo/../qux" === "qux"
  dropDotDot "bar/foo/../qux" === "bar/qux"
  dropDotDot "bar/foo/.." === "bar"

prop_dropDotDot_dotInfix :: Property
prop_dropDotDot_dotInfix = once $ do
  dropDotDot "bar/foo/../../qux" === "qux"
  dropDotDot "bar/foo/../../qux/../foo" === "foo"

prop_dropDotDot_dotPrefix :: Property
prop_dropDotDot_dotPrefix = once $ do
  dropDotDot "../../foo" === "foo"
  dropDotDot "../foo" === "foo"
  dropDotDot "./../foo" === "foo"
  dropDotDot "./foo" === "./foo"

prop_dropDotDot_dotSuffix :: Property
prop_dropDotDot_dotSuffix = once $ do
  dropDotDot "foo/.." === ""
  dropDotDot "foo/bar/.." === "foo"
  dropDotDot "foo/bar/../.." === ""

-- https://github.com/hedgehogqa/haskell-hedgehog/issues/121
once :: PropertyT IO () -> Property
once = withTests 1 . property
