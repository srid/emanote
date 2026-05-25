module Emanote.MCP.ToolsSpec where

import Emanote.MCP.Tools (NoteMatch (..), ResolveResult (..), findNotes, getBacklinks, resolveWikilink)
import Emanote.Model.Note qualified as MN
import Emanote.Model.Type qualified as M
import Emanote.Route.ModelRoute (LMLRoute (LMLRoute_Md))
import Emanote.Route.R (R (..))
import Network.URI.Slug (Slug)
import Relude
import Test.Hspec
import Text.Pandoc.Definition qualified as B

mkModel :: [MN.Note] -> M.Model
mkModel notes =
  M.withRoutePrism (error "route prism unused by Tools") $
    foldr M.modelInsertNote base notes
  where
    base =
      M.emptyModel
        mempty
        (error "CLI action unused")
        (error "renderers unused")
        (error "scripting engine unused")
        False
        False
        (error "instance ID unused")
        (error "stork index unused")

-- A note whose H1 sets the title; the route encodes the source path.
noteAt :: NonEmpty Slug -> Text -> MN.Note
noteAt slugs title =
  MN.mkEmptyNoteWith
    (LMLRoute_Md (R slugs))
    [B.Header 1 B.nullAttr [B.Str title]]

spec :: Spec
spec = do
  describe "findNotes" $ do
    let notebook =
          mkModel
            [ noteAt ("guide" :| ["mcp"]) "MCP server"
            , noteAt ("guide" :| ["wikilinks"]) "Wikilinks"
            , noteAt ("index" :| []) "Home"
            ]

    it "matches case-insensitively against titles" $ do
      let hits = findNotes "wiki" 20 notebook
      fmap path hits `shouldBe` ["guide/wikilinks.md"]

    it "matches case-insensitively against source paths" $ do
      let hits = findNotes "GUIDE/MCP" 20 notebook
      fmap path hits `shouldBe` ["guide/mcp.md"]

    it "honours the limit argument" $ do
      length (findNotes "" 2 notebook) `shouldBe` 2

    it "treats a non-positive limit as zero" $ do
      findNotes "guide" 0 notebook `shouldBe` []

    it "advertises an emanote:// URI for every match" $ do
      let [hit] = findNotes "wiki" 20 notebook
      uri hit `shouldBe` "emanote://note/guide/wikilinks.md"

  describe "getBacklinks" $ do
    let target = LMLRoute_Md (R ("guide" :| ["neuron"]))
        source = LMLRoute_Md (R ("index" :| []))
        sourceNote =
          MN.mkEmptyNoteWith
            source
            [ B.Para
                [ B.Str "See "
                , B.Link B.nullAttr [B.Str "Neuron"] ("guide/neuron", "")
                ]
            ]
        targetNote = MN.mkEmptyNoteWith target [B.Header 1 B.nullAttr [B.Str "Neuron"]]
        notebook = mkModel [sourceNote, targetNote]

    it "returns matching source paths" $ do
      let Right ms = getBacklinks "guide/neuron.md" notebook
      fmap path ms `shouldBe` ["index.md"]

    it "rejects unrecognised paths" $ do
      getBacklinks "" notebook `shouldSatisfy` isLeft

    it "returns an empty list for a known-but-unlinked path" $ do
      getBacklinks "index.md" notebook `shouldBe` Right []

  describe "resolveWikilink" $ do
    let target = LMLRoute_Md (R ("guide" :| ["neuron"]))
        notebook =
          mkModel
            [ MN.mkEmptyNoteWith target [B.Header 1 B.nullAttr [B.Str "Neuron"]]
            , MN.mkEmptyNoteWith (LMLRoute_Md (R ("index" :| []))) []
            ]

    it "resolves a known wikilink to a note match" $ do
      let Right res = resolveWikilink "guide/neuron" Nothing notebook
      case res of
        ResolvedNote nm -> path nm `shouldBe` "guide/neuron.md"
        other -> expectationFailure $ "Unexpected resolve result: " <> show other

    it "reports missing when no candidate exists" $ do
      resolveWikilink "no-such-note" Nothing notebook `shouldBe` Right UnresolvedMissing

    it "rejects an empty wikilink" $ do
      resolveWikilink "" Nothing notebook `shouldSatisfy` isLeft

    it "rejects an unrecognised `from` path" $ do
      resolveWikilink "guide/neuron" (Just "bogus") notebook `shouldSatisfy` isLeft
