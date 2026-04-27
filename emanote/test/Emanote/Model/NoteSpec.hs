module Emanote.Model.NoteSpec where

import Commonmark.Extensions.WikiLink qualified as WL
import Data.IxSet.Typed qualified as Ix
import Data.TagTree qualified as TT
import Emanote.Model.Link.Rel qualified as Rel
import Emanote.Model.Note qualified as MN
import Emanote.Model.Title qualified as Tit
import Emanote.Route qualified as R
import Emanote.Source.Loc (Loc (LocUser))
import Optics.Operators ((^.))
import Relude
import Test.Hspec

spec :: Spec
spec = do
  describe "parseSimpleMarkdownNote" $ do
    it "keeps simple Markdown shallow while preserving title, tags, and wikilinks" $ do
      let note = parseSimpleNote "note.md" simpleMarkdown

      note ^. MN.noteNeedsFullParse `shouldBe` True
      note ^. MN.noteSourceText `shouldBe` Just simpleMarkdown
      Tit.toPlain (note ^. MN.noteTitle) `shouldBe` "Note title"
      MN.noteTags note
        `shouldMatchList` [ TT.Tag "frontmatter"
                          , TT.Tag "body-tag"
                          , TT.Tag "nested/tag"
                          ]
      fmap (^. Rel.relTo) (Ix.toList $ Rel.noteRels note)
        `shouldMatchList` [ Rel.URTWikiLink (WL.WikiLinkNormal, wiki "target")
                          , Rel.URTWikiLink (WL.WikiLinkTag, wiki "tagged")
                          , Rel.URTWikiLink (WL.WikiLinkBranch, wiki "child")
                          ]

    it "leaves Markdown requiring full document semantics to the full parser" $ do
      MN.parseSimpleMarkdownNote (route "linked.md") (source "linked.md") "# Title\n[x](https://example.com)\n"
        `shouldBe` Nothing
      MN.parseSimpleMarkdownNote (route "reference.md") (source "reference.md") "# Title\n[x][ref]\n\n[ref]: other.md\n"
        `shouldBe` Nothing
      MN.parseSimpleMarkdownNote (route "task.md") (source "task.md") "# Title\n- [ ] task\n"
        `shouldBe` Nothing
      MN.parseSimpleMarkdownNote (route "filtered.md") (source "filtered.md") filteredMarkdown
        `shouldBe` Nothing
  where
    simpleMarkdown =
      unlines
        [ "---"
        , "tags: [frontmatter]"
        , "---"
        , "# Note title"
        , "Body #body-tag #nested/tag links to [[target|Target]], #[[tagged]], and [[child]]#."
        ]

    filteredMarkdown =
      unlines
        [ "---"
        , "pandoc:"
        , "  filters: [filter.lua]"
        , "---"
        , "# Title"
        ]

parseSimpleNote :: FilePath -> Text -> MN.Note
parseSimpleNote fp body =
  fromMaybe (error "expected simple Markdown note")
    $ MN.parseSimpleMarkdownNote (route fp) (source fp) body

route :: FilePath -> R.LMLRoute
route fp =
  fromMaybe (error $ "bad route: " <> toText fp)
    $ R.mkLMLRouteFromKnownFilePath R.Md fp

source :: FilePath -> (Loc, FilePath)
source fp =
  (LocUser 1 "/tmp/emanote-test" Nothing, fp)

wiki :: Text -> WL.WikiLink
wiki slug =
  WL.mkWikiLinkFromSlugs (fromString (toString slug) :| [])
