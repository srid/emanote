module Emanote.Model.Link.RelSpec where

import Data.Aeson qualified as Aeson
import Data.IxSet.Typed qualified as Ix
import Emanote.Model.Link.Rel
import Emanote.Model.Note qualified as MN
import Emanote.Pandoc.Markdown.Parser (parseMarkdown)
import Emanote.Route.ModelRoute (LMLRoute (LMLRoute_Md), mkModelRouteCandidates)
import Emanote.Route.R (R (..))
import Emanote.Route.R qualified as R
import Hedgehog
import Optics.Operators ((^.))
import Relude
import Test.Hspec
import Test.Hspec.Hedgehog
import Text.Pandoc.Definition qualified as B
import Text.Pandoc.Walk qualified as W

spec :: Spec
spec = do
  describe "noteRels" $ do
    it "preserves repeated link contexts in Markdown source order (issue #186)" $ do
      let note =
            parseTestNote
              $ unlines
                [ "b [[Foo]]"
                , ""
                , "a [[Foo]]"
                , ""
                , "c [[Foo]]"
                ]
      fmap relContextFirstWord (Ix.toList $ noteRels note)
        `shouldBe` ["b", "a", "c"]
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

parseTestNote :: Text -> MN.Note
parseTestNote body =
  case parseMarkdown "<test>" body of
    Left err -> error err
    Right (_meta, doc) ->
      MN.mkNoteWith noteRoute Nothing doc Aeson.Null mempty
  where
    noteRoute =
      LMLRoute_Md
        $ fromMaybe (error "bad route")
        $ R.mkRouteFromFilePath "source.md"

relContextFirstWord :: Rel -> Text
relContextFirstWord rel =
  fromMaybe (error "missing context word")
    $ viaNonEmpty head
    $ W.query getStr
    $ rel
    ^. relCtx
  where
    getStr = \case
      B.Str x -> [x]
      _ -> []
