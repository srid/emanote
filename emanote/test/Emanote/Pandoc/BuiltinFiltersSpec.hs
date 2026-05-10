module Emanote.Pandoc.BuiltinFiltersSpec where

import Commonmark.Extensions.WikiLink (plainify)
import Emanote.Pandoc.BuiltinFilters (preparePandoc)
import Emanote.Pandoc.Markdown.Parser (parseMarkdown)
import Emanote.Pandoc.Markdown.Syntax.HashTag qualified as HT
import Relude
import System.Directory (doesFileExist)
import Test.Hspec
import Text.Pandoc.Definition (Inline (Link), Pandoc)
import Text.Pandoc.Walk qualified as W

spec :: Spec
spec = do
  describe "docs regression coverage" $ do
    it "keeps the Lua filters phase-table wikilink inside its table cell" $ do
      source <- readFirstExisting ["docs/guide/lua-filters.md", "../docs/guide/lua-filters.md"]
      links source `shouldSatisfy` elem ("html-template", "HTML-specific")

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
    it "does not treat GitHub issue references as inline tags"
      $ links "mentions #221, (#228), and #263)"
      `shouldBe` []
    it "does not extract GitHub issue references as body tags"
      $ tags "mentions #221, (#228), and [#263](https://github.com/srid/emanote/issues/263)"
      `shouldBe` []
    it "still extracts tags that contain digits"
      $ tags "tagged #foo #foo/bar #tag221 ###structure #§1"
      `shouldBe` ["foo", "foo/bar", "tag221", "##structure", "§1"]

links :: Text -> [(Text, Text)]
links = either error (collect . preparePandoc . snd) . parseMarkdown "<test>"
  where
    collect :: Pandoc -> [(Text, Text)]
    collect = W.query $ \case
      Link _ inlines (url, _) -> [(url, plainify inlines)]
      _ -> []

tags :: Text -> [Text]
tags =
  either error (fmap (\(HT.Tag tag) -> tag) . HT.inlineTagsInPandoc . preparePandoc . snd)
    . parseMarkdown "<test>"

readFirstExisting :: [FilePath] -> IO Text
readFirstExisting = \case
  [] -> error "Could not find docs/guide/lua-filters.md from test working directory"
  fp : rest -> do
    exists <- doesFileExist fp
    if exists then decodeUtf8 <$> readFileBS fp else readFirstExisting rest
