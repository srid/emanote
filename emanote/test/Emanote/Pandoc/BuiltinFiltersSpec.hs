module Emanote.Pandoc.BuiltinFiltersSpec where

import Commonmark.Extensions.WikiLink (plainify)
import Emanote.Pandoc.BuiltinFilters (flattenNestedLinks)
import Emanote.Pandoc.Markdown.Parser (parseMarkdown)
import Relude
import Test.Hspec
import Text.Pandoc.Definition (Inline (Link), Pandoc)
import Text.Pandoc.Walk qualified as W

spec :: Spec
spec = do
  -- Regression test for https://github.com/srid/emanote/issues/349.
  -- `flattenNestedLinks` lives in BuiltinFilters and runs in
  -- `Emanote.Model.Note.parseNoteMarkdown` *after* user-defined Pandoc
  -- filters, so a user filter that produces a Link-in-Link cannot bypass it.
  -- The spec pins the function's behaviour against the real `parseMarkdown`
  -- output for the cases from the bug report.
  describe "flattenNestedLinks unwraps URL-bearing link labels (#349)" $ do
    it "URL-only label"
      $ links "[https://www.website.com](https://www.website.com#something)"
      `shouldBe` [("https://www.website.com#something", "https://www.website.com")]
    it "mailto label"
      $ links "[nobody@example.com](mailto:nobody@example.com?subject=Some%20subject)"
      `shouldBe` [("mailto:nobody@example.com?subject=Some%20subject", "nobody@example.com")]
    it "sentence label containing two URLs"
      $ links "[A website similar to https://www.foo.com and https://www.bar.com](https://www.baz.com)"
      `shouldBe` [("https://www.baz.com", "A website similar to https://www.foo.com and https://www.bar.com")]
    it "URL inside emphasis inside a label"
      $ links "[**https://www.example.com**](http://target.com)"
      `shouldBe` [("http://target.com", "https://www.example.com")]

links :: Text -> [(Text, Text)]
links = either error (collect . flattenNestedLinks . snd) . parseMarkdown "<test>"
  where
    collect :: Pandoc -> [(Text, Text)]
    collect = W.query $ \case
      Link _ inlines (url, _) -> [(url, plainify inlines)]
      _ -> []
