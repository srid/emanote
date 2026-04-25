module Emanote.Pandoc.MermaidSpec where

import Control.Monad.Writer.Strict (runWriterT)
import Emanote.Pandoc.Mermaid (stripXmlPrologue, transformMermaidBlocks)
import Hedgehog
import Relude
import Test.Hspec
import Test.Hspec.Hedgehog
import Text.Pandoc.Definition qualified as B

plainBlock :: B.Block
plainBlock = B.CodeBlock ("", ["haskell"], []) "id = \\x -> x"

doc :: [B.Block] -> B.Pandoc
doc = B.Pandoc mempty

spec :: Spec
spec = do
  describe "stripXmlPrologue" $ do
    it "drops the XML declaration" . hedgehog $ do
      stripXmlPrologue "<?xml version=\"1.0\"?>\n<svg></svg>" === "<svg></svg>"
    it "drops other processing instructions (e.g. xml-stylesheet)" . hedgehog $ do
      stripXmlPrologue "<?xml-stylesheet href=\"x.css\"?>\n<svg/>" === "<svg/>"
    it "drops the DOCTYPE" . hedgehog $ do
      stripXmlPrologue "<!DOCTYPE svg PUBLIC \"...\" \"...\">\n<svg></svg>" === "<svg></svg>"
    it "drops both prologues, in either order" . hedgehog $ do
      stripXmlPrologue "<?xml version=\"1.0\"?>\n<!DOCTYPE svg>\n<svg/>" === "<svg/>"
    it "passes through SVG with no prologue" . hedgehog $ do
      stripXmlPrologue "<svg/>" === "<svg/>"
    it "leaves malformed prologues (no terminator) untouched" . hedgehog $ do
      stripXmlPrologue "<?xml unterminated" === "<?xml unterminated"
      stripXmlPrologue "<!DOCTYPE no-close" === "<!DOCTYPE no-close"

  describe "transformMermaidBlocks" $ do
    it "is a no-op when no mermaid blocks are present" $ do
      -- A document without mermaid blocks must not invoke mmdc at all
      -- (verified by passing through unchanged with no errors `tell`'d).
      (result, errs) <- runWriterT $ transformMermaidBlocks (doc [plainBlock])
      result `shouldBe` doc [plainBlock]
      errs `shouldBe` []

-- The success path (mmdc actually invoked) lives in the e2e suite at
-- tests/features/smoke.feature, where a built emanote with the Nix-supplied
-- mmdc renders a fixture diagram and the resulting page is asserted to
-- contain an inline <svg> element.
