module Emanote.Pandoc.Renderer.CalloutSpec where

import Emanote.Pandoc.Renderer.Callout
import Hedgehog
import Relude
import Test.Hspec
import Test.Hspec.Hedgehog
import Text.Pandoc.Definition qualified as B

spec :: Spec
spec = do
  describe "callout type parsing" $ do
    it "parses bare types and normalises case" . hedgehog $ do
      parseCalloutType "[!tip]" === Just (CalloutType "tip")
      parseCalloutType "[!Note]" === Just (CalloutType "note")
      parseCalloutType "[!INFO]" === Just (CalloutType "info")

  describe "callout header parsing (with fold suffix)" $ do
    it "parses non-foldable headers" . hedgehog $ do
      parseCalloutHeader "[!tip]" === Just (CalloutType "tip", Nothing)
      parseCalloutHeader "[!Note]" === Just (CalloutType "note", Nothing)
    it "parses foldable+expanded headers (`+` suffix)" . hedgehog $ do
      parseCalloutHeader "[!tip]+" === Just (CalloutType "tip", Just Expanded)
      parseCalloutHeader "[!warning]+" === Just (CalloutType "warning", Just Expanded)
    it "parses foldable+collapsed headers (`-` suffix)" . hedgehog $ do
      parseCalloutHeader "[!tip]-" === Just (CalloutType "tip", Just Collapsed)
      parseCalloutHeader "[!INFO]-" === Just (CalloutType "info", Just Collapsed)
    it "rejects unknown suffixes" . hedgehog $ do
      parseCalloutHeader "[!tip]?" === Nothing
      parseCalloutHeader "[!tip]++" === Nothing

  describe "parseCallout (block-level)" $ do
    it "parses a basic callout" . hedgehog $ do
      let blks = [B.Para [B.Str "[!tip]", B.Space, B.Str "Hello"]]
      (type_ <$> parseCallout blks) === Just (CalloutType "tip")
      (foldState <$> parseCallout blks) === Just Nothing

    it "parses a foldable+collapsed callout (suffix in same Str token)" . hedgehog $ do
      let blks = [B.Para [B.Str "[!tip]-", B.Space, B.Str "Click me"]]
      (foldState <$> parseCallout blks) === Just (Just Collapsed)

    it "parses a foldable suffix split across tokens (defensive case)" . hedgehog $ do
      -- If pandoc happens to tokenise `[!tip]+` as two adjacent Str nodes,
      -- the parser must still recognise the fold suffix.
      let blks = [B.Para [B.Str "[!tip]", B.Str "+", B.Space, B.Str "Hi"]]
      (foldState <$> parseCallout blks) === Just (Just Expanded)

    it "preserves a nested blockquote in the body" . hedgehog $ do
      -- Outer `[!note]` containing an inner `[!tip]` blockquote. The outer
      -- parse should yield the inner BlockQuote intact in `body`, so the
      -- recursive renderer can turn it into a nested callout.
      let inner = B.BlockQuote [B.Para [B.Str "[!tip]", B.Space, B.Str "Inner"]]
          blks = [B.Para [B.Str "[!note]", B.Space, B.Str "Outer"], inner]
          outer = parseCallout blks
      (type_ <$> outer) === Just (CalloutType "note")
      (body <$> outer) === Just [inner]

    it "rejects a non-callout blockquote" . hedgehog $ do
      let blks = [B.Para [B.Str "Just", B.Space, B.Str "a", B.Space, B.Str "quote"]]
      parseCallout blks === Nothing
