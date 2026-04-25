module Emanote.View.FeedSpec where

import Data.Text.Lazy qualified as LT
import Emanote.Model.Note (mkEmptyNoteWith)
import Emanote.Model.Query (Query (QueryByPathPattern))
import Emanote.Route qualified as R
import Emanote.Route.ModelRoute (LMLRoute (LMLRoute_Md))
import Emanote.View.Feed (getNoteQuery, renderEmptyFeed)
import Relude
import Test.Hspec
import Text.Pandoc.Definition

spec :: Spec
spec = do
  describe "getNoteQuery" $ do
    it "returns Left on a note with no query block" $ do
      let note = mkEmptyNoteWith lmlRoute [Para [Str "just prose, no query"]]
      getNoteQuery note `shouldBe` Left "can't find note query"
    it "returns Left on an empty note" $ do
      let note = mkEmptyNoteWith lmlRoute []
      getNoteQuery note `shouldBe` Left "empty note"
    it "returns Left on a note whose query is syntactically invalid" $ do
      let note = mkEmptyNoteWith lmlRoute [queryBlock "not-a-valid-query"]
      getNoteQuery note `shouldBe` Left "invalid query: not-a-valid-query"
    it "returns Left on a note with multiple query blocks" $ do
      let note = mkEmptyNoteWith lmlRoute [queryBlock "path:./*", queryBlock "path:./*"]
      getNoteQuery note `shouldBe` Left "multiple ```query found"
    it "returns Right on a note with a single valid query" $ do
      let note = mkEmptyNoteWith lmlRoute [queryBlock "path:./*"]
      getNoteQuery note `shouldBe` Right (QueryByPathPattern "./*")

  describe "renderEmptyFeed" $ do
    it "produces valid Atom XML for an empty query result (regression: issue #490)" $ do
      let note = mkEmptyNoteWith lmlRoute [queryBlock "path:./does-not-exist/*"]
          xml = renderEmptyFeed note
      LT.take 5 xml `shouldBe` "<?xml"
      ("<feed" `LT.isInfixOf` xml) `shouldBe` True

lmlRoute :: LMLRoute
lmlRoute = LMLRoute_Md $ fromMaybe (error "bad route") $ R.mkRouteFromFilePath "blog.md"

queryBlock :: Text -> Block
queryBlock = CodeBlock ("", ["query"], [])
