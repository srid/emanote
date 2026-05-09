module Emanote.View.LintTemplateSpec where

import Emanote.View.LintTemplate (UnboundSplice (..), formatWarning, scanRenderedHtml)
import Relude
import Test.Hspec

spec :: Spec
spec = do
  describe "scanRenderedHtml" $ do
    it "ignores well-rendered HTML with no splice survivors" $ do
      scanRenderedHtml "ok.html" "<html><body><p>hello</p></body></html>"
        `shouldBe` Right []

    it "reports an element whose tag name still has a Heist-style colon" $ do
      scanRenderedHtml "ok.html" "<html><body><ema:tite></ema:tite></body></html>"
        `shouldBe` Right [SpliceElement "ema:tite"]

    it "reports nested unbound element splices" $ do
      scanRenderedHtml "ok.html" "<html><body><div><ema:foo>x</ema:foo></div></body></html>"
        `shouldBe` Right [SpliceElement "ema:foo"]

    it "reports a literal ${name} that survived as an attribute value" $ do
      scanRenderedHtml "ok.html" "<html><body><a href=\"${value:siteUrl}\">x</a></body></html>"
        `shouldBe` Right [SpliceAttribute "value:siteUrl"]

    it "reports multiple ${...} references in one attribute" $ do
      scanRenderedHtml "ok.html" "<html><body><img alt=\"${a} - ${b}\"/></body></html>"
        `shouldBe` Right [SpliceAttribute "a", SpliceAttribute "b"]

    it "deduplicates repeated occurrences across the document" $ do
      scanRenderedHtml "ok.html" "<html><body><ema:foo></ema:foo><ema:foo></ema:foo></body></html>"
        `shouldBe` Right [SpliceElement "ema:foo"]

    it "leaves text nodes alone (e.g. ${...} appearing inside a <pre>)" $ do
      scanRenderedHtml "ok.html" "<html><body><pre>${not-an-attr}</pre></body></html>"
        `shouldBe` Right []

    it "still reports a valid ${...} that follows an unbalanced ${" $ do
      scanRenderedHtml "ok.html" "<html><body><a title=\"${incomplete ${valid}\">x</a></body></html>"
        `shouldBe` Right [SpliceAttribute "valid"]

    it "ignores a trailing ${ with no closing brace" $ do
      scanRenderedHtml "ok.html" "<html><body><a title=\"${valid} ${trailing\">x</a></body></html>"
        `shouldBe` Right [SpliceAttribute "valid"]

    it "skips the parse entirely when the bytes have no splice marker" $ do
      -- The pre-check short-circuits to Right [] without invoking parseHTML,
      -- so even malformed HTML returns clean — there is nothing for the lint
      -- to find when the bytes contain neither '${' nor a colon-tag.
      scanRenderedHtml "bad.html" "<html><body><div></span></body></html>"
        `shouldBe` Right []

  describe "formatWarning" $ do
    it "formats element warnings as a tag" $ do
      formatWarning (SpliceElement "ema:tite") `shouldBe` "<ema:tite/>"

    it "formats attribute warnings as a dollar-brace token" $ do
      formatWarning (SpliceAttribute "value:siteUrl") `shouldBe` "${value:siteUrl}"
