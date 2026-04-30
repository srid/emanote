module Emanote.Model.Link.RelSpec where

import Commonmark.Extensions.WikiLink qualified as WL
import Data.IxSet.Typed qualified as Ix
import Emanote.Model.Link.Rel
import Emanote.Model.Note qualified as MN
import Emanote.Route qualified as R
import Emanote.Route.ModelRoute (LMLRoute (LMLRoute_Md), mkModelRouteCandidates)
import Emanote.Route.R (R (..))
import Hedgehog
import Relude
import Test.Hspec
import Test.Hspec.Hedgehog
import Text.Pandoc.Definition qualified as B

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
  describe "noteRels source order (issue #186)" $ do
    it "orders rels by source position, not by lexicographic Ord on context" $ do
      -- 'Z' sorts last lexicographically but comes first in source; 'A'
      -- sorts first but comes second. Without the srcPos tie-breaker,
      -- Ord [Block] would yield A-then-Z; we want source order.
      let mkLink lbl = B.Link B.nullAttr [B.Str lbl] ("Foo.md", "")
          note =
            MN.mkEmptyNoteWith
              barRoute
              [ B.Para [B.Str "Z first: ", mkLink "z"]
              , B.Para [B.Str "A second: ", mkLink "a"]
              ]
          paraText rel = case _relCtx rel of
            [B.Para is] -> WL.plainify is
            other -> error $ "expected single-paragraph context, got " <> show other
      (paraText <$> Ix.toList (noteRels note))
        `shouldBe` ["Z first: z", "A second: a"]
    it "does not collapse two identical-context links to the same target" $ do
      -- One paragraph mentions Foo.md twice. The two rels share
      -- (relFrom, relTo, relCtx); without srcPos in Ord, IxSet.fromList's
      -- set-dedup would collapse them to one.
      let mkLink = B.Link B.nullAttr [B.Str "Foo"] ("Foo.md", "")
          note =
            MN.mkEmptyNoteWith
              barRoute
              [B.Para [B.Str "See ", mkLink, B.Str " and ", mkLink, B.Str "."]]
      length (Ix.toList (noteRels note)) `shouldBe` 2

barRoute :: LMLRoute
barRoute = LMLRoute_Md $ fromMaybe (error "barRoute: Bar.md failed to parse") $ R.mkRouteFromFilePath "Bar.md"

-- https://github.com/hedgehogqa/haskell-hedgehog/issues/121
once :: PropertyT IO () -> Property
once = withTests 1 . property
