module Emanote.Model.Link.RelSpec where

import Emanote.Model.Link.Rel
import Relude
import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree
spec =
  testGroup
    "Rel"
    [ dropDotDot_spec
    ]

dropDotDot_spec :: TestTree
dropDotDot_spec =
  testGroup
    "dropDotDot"
    [ testCase "simple" $ do
        dropDotDot "foo/bar/qux" @?= "foo/bar/qux"
        dropDotDot "foo/../qux" @?= "qux"
        dropDotDot "bar/foo/../qux" @?= "bar/qux"
        dropDotDot "bar/foo/.." @?= "bar",
      testCase "complex" $ do
        dropDotDot "bar/foo/../../qux" @?= "qux"
        dropDotDot "bar/foo/../../qux/../foo" @?= "foo",
      testCase "beginning" $ do
        dropDotDot "../../foo" @?= "foo"
        dropDotDot "../foo" @?= "foo"
        dropDotDot "./../foo" @?= "foo"
        dropDotDot "./foo" @?= "./foo",
      testCase "end" $ do
        dropDotDot "foo/.." @?= ""
        dropDotDot "foo/bar/.." @?= "foo"
        dropDotDot "foo/bar/../.." @?= ""
    ]
