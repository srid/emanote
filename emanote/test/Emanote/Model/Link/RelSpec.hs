module Emanote.Model.Link.RelSpec where

import Emanote.Model.Link.Rel
import Emanote.Model.Note qualified as MN
import Emanote.Route.ModelRoute (mkModelRouteCandidates)
import Emanote.Route.R (R (..))
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
  describe "resolveLinkBaseFromFilePath" $ do
    it "top-level files have no base" . hedgehog $ do
      MN.resolveLinkBaseFromFilePath "index.md" === Nothing
      MN.resolveLinkBaseFromFilePath "foo.md" === Nothing
    it "subfolder/index.md uses subfolder as base (issue #608)" . hedgehog $ do
      MN.resolveLinkBaseFromFilePath "subfolder/index.md"
        === Just (R ("subfolder" :| []))
    it "subfolder/foo.md uses subfolder as base" . hedgehog $ do
      MN.resolveLinkBaseFromFilePath "subfolder/foo.md"
        === Just (R ("subfolder" :| []))
    it "a/b/index.md uses a/b as base" . hedgehog $ do
      MN.resolveLinkBaseFromFilePath "a/b/index.md"
        === Just (R ("a" :| ["b"]))
  describe "parseUnresolvedRelTarget (issue #608)" $ do
    it "relative link from subfolder/index.md resolves under subfolder/" . hedgehog $ do
      let base = MN.resolveLinkBaseFromFilePath "subfolder/index.md"
          got = fst <$> parseUnresolvedRelTarget base [] "foo.md"
          want = viaNonEmpty URTResource (mkModelRouteCandidates "subfolder/foo.md")
      got === want

-- https://github.com/hedgehogqa/haskell-hedgehog/issues/121
once :: PropertyT IO () -> Property
once = withTests 1 . property
