module Emanote.Pandoc.MermaidSpec where

import Control.Exception (bracket_)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Writer.Strict (runWriterT)
import Emanote.Pandoc.Mermaid (stripXmlPrologue, transformMermaidBlocks)
import Hedgehog
import Relude
import System.Environment (setEnv, unsetEnv)
import Test.Hspec
import Test.Hspec.Hedgehog
import Text.Pandoc.Definition qualified as B

runMermaid :: B.Pandoc -> IO (B.Pandoc, [Text])
runMermaid = runNoLoggingT . runWriterT . transformMermaidBlocks

mermaidBlock :: B.Block
mermaidBlock = B.CodeBlock ("", ["mermaid"], []) "graph LR\nA --> B"

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
      (result, errs) <- runMermaid (doc [plainBlock])
      result `shouldBe` doc [plainBlock]
      errs `shouldBe` []
    it "preserves mermaid blocks when mmdc is missing from PATH" $ do
      -- Point PATH at a directory that exists but holds no mmdc.
      -- (`setEnv name ""` in base actually *unsets*, so we need a real path.)
      withMmdcMissing $ do
        (result, errs) <- runMermaid (doc [mermaidBlock])
        result `shouldBe` doc [mermaidBlock]
        -- Missing mmdc is environmental, not a per-note error: do not tell it.
        errs `shouldBe` []

withMmdcMissing :: IO a -> IO a
withMmdcMissing action = do
  original <- lookupEnv "PATH"
  bracket_ (setEnv "PATH" "/var/empty") (restore original) action
  where
    restore = maybe (unsetEnv "PATH") (setEnv "PATH")
