module Emanote.Model.GraphSpec where

import Data.Aeson qualified as Aeson
import Data.Default (def)
import Ema.CLI qualified as Ema
import Emanote.Model.Graph qualified as G
import Emanote.Model.Note qualified as MN
import Emanote.Model.Type qualified as M
import Emanote.Pandoc.Markdown.Parser (parseMarkdown)
import Emanote.Route.ModelRoute (LMLRoute (LMLRoute_Md))
import Emanote.Route.R qualified as R
import Optics.Operators ((^.))
import Relude
import Test.Hspec
import Text.Pandoc.Definition qualified as B
import Text.Pandoc.Walk qualified as W

spec :: Spec
spec = do
  describe "modelLookupBacklinks" $ do
    it "preserves repeated backlink contexts in source order (issue #186)" $ do
      let target = parseTestNoteWith "foo.md" "# Foo"
          source =
            parseTestNoteWith "source.md"
              $ unlines
                [ "b [[foo]]"
                , ""
                , "a [[foo]]"
                , ""
                , "c [[foo]]"
                ]
          backlinks = G.modelLookupBacklinks (target ^. MN.noteRoute) $ testModel [target, source]
          contexts =
            case backlinks of
              [(sourceRoute, ctxs)]
                | sourceRoute == source ^. MN.noteRoute ->
                    toList ctxs
              _ ->
                error "unexpected backlinks"

      fmap contextFirstWord contexts `shouldBe` ["b", "a", "c"]

testModel :: [MN.Note] -> M.Model
testModel notes =
  M.withRoutePrism (error "unused route prism")
    $ flipfoldl' M.modelInsertNote emptyModel notes
  where
    emptyModel =
      M.emptyModel
        mempty
        (Ema.action def)
        (error "unused renderers")
        False
        (error "unused uuid")
        (error "unused stork index")

parseTestNoteWith :: FilePath -> Text -> MN.Note
parseTestNoteWith fp body =
  case parseMarkdown fp body of
    Left err -> error err
    Right (_meta, doc) ->
      MN.mkNoteWith noteRoute Nothing doc Aeson.Null mempty
  where
    noteRoute =
      LMLRoute_Md
        $ fromMaybe (error "bad route")
        $ R.mkRouteFromFilePath fp

contextFirstWord :: [B.Block] -> Text
contextFirstWord blocks =
  fromMaybe (error "missing context word")
    $ viaNonEmpty head
    $ W.query getStr blocks
  where
    getStr = \case
      B.Str x -> [x]
      _ -> []
