module Emanote.Pandoc.MermaidSpec where

import Control.Exception (bracket_)
import Control.Monad.Logger (runNoLoggingT)
import Emanote.Pandoc.Mermaid (hasMermaidBlock, stripXmlPrologue, transformMermaidBlocks)
import Hedgehog
import Relude
import System.Environment (setEnv, unsetEnv)
import Test.Hspec
import Test.Hspec.Hedgehog
import Text.Pandoc.Definition qualified as B

mermaidBlock :: B.Block
mermaidBlock = B.CodeBlock ("", ["mermaid"], []) "graph LR\nA --> B"

plainBlock :: B.Block
plainBlock = B.CodeBlock ("", ["haskell"], []) "id = \\x -> x"

doc :: [B.Block] -> B.Pandoc
doc = B.Pandoc mempty

spec :: Spec
spec = do
  describe "hasMermaidBlock" $ do
    it "detects a CodeBlock with the mermaid class" . hedgehog $ do
      hasMermaidBlock (doc [mermaidBlock]) === True
    it "ignores CodeBlocks without the mermaid class" . hedgehog $ do
      hasMermaidBlock (doc [plainBlock]) === False
    it "ignores documents with no code blocks" . hedgehog $ do
      hasMermaidBlock (doc [B.Para [B.Str "hi"]]) === False

  describe "stripXmlPrologue" $ do
    it "drops the XML declaration" . hedgehog $ do
      stripXmlPrologue "<?xml version=\"1.0\"?>\n<svg></svg>" === "<svg></svg>"
    it "drops the DOCTYPE" . hedgehog $ do
      stripXmlPrologue "<!DOCTYPE svg PUBLIC \"...\" \"...\">\n<svg></svg>" === "<svg></svg>"
    it "drops both prologues, in either order" . hedgehog $ do
      stripXmlPrologue "<?xml version=\"1.0\"?>\n<!DOCTYPE svg>\n<svg/>" === "<svg/>"
    it "passes through SVG with no prologue" . hedgehog $ do
      stripXmlPrologue "<svg/>" === "<svg/>"

  describe "transformMermaidBlocks" $ do
    it "is a no-op when no mermaid blocks are present" $ do
      result <- runNoLoggingT $ transformMermaidBlocks (doc [plainBlock])
      result `shouldBe` doc [plainBlock]
    it "preserves mermaid blocks when mmdc is missing from PATH" $ do
      -- Force findExecutable to return Nothing by clobbering PATH.
      withEmptyPath $ do
        result <- runNoLoggingT $ transformMermaidBlocks (doc [mermaidBlock])
        result `shouldBe` doc [mermaidBlock]

withEmptyPath :: IO a -> IO a
withEmptyPath action = do
  original <- lookupEnv "PATH"
  bracket_ (setEnv "PATH" "") (restore original) action
  where
    restore = maybe (unsetEnv "PATH") (setEnv "PATH")
