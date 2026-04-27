module Emanote.Pandoc.Markdown.ParserSpec where

import Commonmark.Extensions.WikiLink qualified as WL
import Emanote.Pandoc.Markdown.Parser (parseMarkdown)
import Hedgehog
import Relude
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)
import Text.Pandoc.Definition qualified as B
import Text.Pandoc.Walk qualified as W

spec :: Spec
spec = do
  describe "parseMarkdown" $ do
    it "does not keep autolinks nested inside link descriptions" . hedgehog $ do
      linkTextsAndTargets issue349Markdown
        === Right
          [ ("https://www.website.com", "https://www.website.com#something")
          , ("nobody@example.com", "mailto:nobody@example.com?subject=Some%20subject")
          , ("A website similar to https://www.foo.com and https://www.bar.com", "https://www.baz.com")
          ]

issue349Markdown :: Text
issue349Markdown =
  unlines
    [ "[https://www.website.com](https://www.website.com#something)"
    , ""
    , "[nobody@example.com](mailto:nobody@example.com?subject=Some%20subject)"
    , ""
    , "[A website similar to https://www.foo.com and https://www.bar.com](https://www.baz.com)"
    ]

linkTextsAndTargets :: Text -> Either Text [(Text, Text)]
linkTextsAndTargets =
  fmap (W.query linkTextAndTarget . snd) . parseMarkdown "<test>"
  where
    linkTextAndTarget :: B.Inline -> [(Text, Text)]
    linkTextAndTarget (B.Link _ inlines (url, _)) =
      [(WL.plainify inlines, url)]
    linkTextAndTarget _ =
      []
