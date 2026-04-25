module Emanote.Pandoc.MermaidSpec where

import Control.Monad.Logger (runNoLoggingT)
import Data.Aeson qualified as Aeson
import Emanote.Pandoc.Mermaid (stripXmlPrologue, transformMermaidBlocks)
import Hedgehog
import Relude
import Test.Hspec
import Test.Hspec.Hedgehog
import Text.Pandoc.Definition qualified as B

plainBlock :: B.Block
plainBlock = B.CodeBlock ("", ["haskell"], []) "id = \\x -> x"

mermaidBlock :: B.Block
mermaidBlock = B.CodeBlock ("", ["mermaid"], []) "graph LR\nA --> B"

doc :: [B.Block] -> B.Pandoc
doc = B.Pandoc mempty

emptyMeta :: Aeson.Value
emptyMeta = Aeson.object []

staticOffMeta :: Aeson.Value
staticOffMeta = Aeson.object ["mermaid" Aeson..= Aeson.object ["static" Aeson..= False]]

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
      -- A document without mermaid blocks must not invoke mmdc at all.
      result <- runNoLoggingT $ transformMermaidBlocks emptyMeta (doc [plainBlock])
      result `shouldBe` doc [plainBlock]

    it "is a no-op when mermaid.static is set to false in the page meta" $ do
      -- Opt-out for client-side rendering. Mermaid blocks must reach the
      -- HTML untouched so the js.mermaid snippet can pick them up.
      result <- runNoLoggingT $ transformMermaidBlocks staticOffMeta (doc [mermaidBlock])
      result `shouldBe` doc [mermaidBlock]

-- The success path (mmdc actually invoked) lives in the e2e suite at
-- tests/features/smoke.feature, where a built emanote with the Nix-supplied
-- mmdc renders a fixture diagram and the resulting page is asserted to
-- contain an inline <svg> element.
