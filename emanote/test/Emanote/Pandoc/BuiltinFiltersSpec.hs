module Emanote.Pandoc.BuiltinFiltersSpec where

import Commonmark.Extensions.WikiLink (plainify)
import Emanote.Pandoc.BuiltinFilters (preparePandoc)
import Emanote.Pandoc.Markdown.Parser (parseMarkdown)
import Relude
import Test.Hspec
import Text.Pandoc.Definition (Inline (Link), Pandoc)
import Text.Pandoc.Walk qualified as W

spec :: Spec
spec = do
  -- Regression test for https://github.com/srid/emanote/issues/349.
  -- `preparePandoc`'s last pass (`flattenNestedLinks`) unwraps any Link
  -- nested inside a parent Link's label — the autolink-extension artifact
  -- the bug describes. The spec pins the behaviour at the `preparePandoc`
  -- boundary, which is the AST handed to renderers.
  describe "preparePandoc flattens URL-bearing link labels (#349)" $ do
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
  -- Regression test for https://github.com/srid/emanote/issues/199.
  -- `linkifyInlineTags` rewrites inline `#tag` syntax into a Link to the
  -- tag-index page. The URL it produces must percent-encode each path
  -- segment, otherwise tags containing reserved URL characters (notably
  -- `#`, used for Zettelkasten "structure note" tags like `##§1`) emit
  -- a href the browser truncates at the literal `#` and treats as an
  -- HTML fragment.
  describe "linkifyInlineTags percent-encodes special tag URLs (#199)" $ do
    -- Wrap each tag in a leading word so commonmark parses it as an inline
    -- (a bare `#tag` at start of line would be parsed as an ATX heading,
    -- never reaching `linkifyInlineTags`).
    it "ascii tag"
      $ links "tagged #foo"
      `shouldBe` [("-/tags/foo.html", "#foo")]
    it "hierarchical tag"
      $ links "tagged #foo/bar"
      `shouldBe` [("-/tags/foo/bar.html", "#foo/bar")]
    it "structure-note tag containing a literal hash"
      $ links "tagged ###structure"
      `shouldBe` [("-/tags/%23%23structure.html", "###structure")]
    it "tag with non-ASCII characters"
      $ links "tagged #§1"
      `shouldBe` [("-/tags/%C2%A71.html", "#§1")]

links :: Text -> [(Text, Text)]
links = either error (collect . preparePandoc . snd) . parseMarkdown "<test>"
  where
    collect :: Pandoc -> [(Text, Text)]
    collect = W.query $ \case
      Link _ inlines (url, _) -> [(url, plainify inlines)]
      _ -> []
