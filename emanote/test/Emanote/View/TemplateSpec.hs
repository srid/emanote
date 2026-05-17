module Emanote.View.TemplateSpec where

import Data.Text qualified as T
import Emanote.Pandoc.Diagnostic qualified as Diagnostic
import Emanote.View.Template (extractInPlaceFilterErrors)
import Relude
import Test.Hspec
import Text.Pandoc.Builder qualified as B
import Text.Pandoc.Definition (Pandoc (..))

-- The bundled `lua-filters/diagram.lua` emits an in-place error block
-- on engine failure (typst/d2/mmdc stderr). The block is a `pandoc.Div`
-- carrying `emanote:error` plus the variant class `emanote:error:lua-filter`,
-- with the engine stderr as its first `CodeBlock` child. The Haskell side
-- (`extractInPlaceFilterErrors`) lifts those text bodies into the
-- `failOnStaticRenderFilterErrors` channel so `emanote gen` aborts on a
-- broken fence instead of silently shipping the unrenderable page.
--
-- These tests pin that contract — what the Lua emits, what the Haskell
-- reader extracts — so a future tweak to either side either preserves
-- the cross-language coupling or breaks loudly here. The class strings
-- come from 'Emanote.Pandoc.Diagnostic' so a rename of the protocol
-- category propagates here without manual edits.

luaFilterClasses :: [Text]
luaFilterClasses = Diagnostic.errorClasses Diagnostic.luaFilterCategory

luaFilterVariantClass :: Text
luaFilterVariantClass = Diagnostic.errorVariantClass Diagnostic.luaFilterCategory

spec :: Spec
spec = describe "extractInPlaceFilterErrors" $ do
  it "extracts the first CodeBlock's text from a marker Div" $ do
    let doc =
          Pandoc mempty $
            one $
              B.Div
                ("", luaFilterClasses, [])
                [ B.Para [B.Strong [B.Str "Diagram error (cetz)"]]
                , B.CodeBlock B.nullAttr "typst: error: unclosed delimiter"
                , B.CodeBlock ("", ["cetz"], []) "#canvas({ broken"
                ]
    extractInPlaceFilterErrors doc `shouldBe` ["typst: error: unclosed delimiter"]

  it "names the marker class when the Div has no CodeBlock" $ do
    -- A future Lua filter could emit the class without a stderr CodeBlock;
    -- the fallback message has to be locatable, not the generic banner
    -- string `prependRenderFilterErrors` uses for catastrophic failures.
    let doc =
          Pandoc mempty $
            one $
              B.Div
                ("", [luaFilterVariantClass], [])
                [B.Para [B.Str "no engine output"]]
        msg = fromMaybe "" $ viaNonEmpty head $ extractInPlaceFilterErrors doc
    msg `shouldSatisfy` T.isInfixOf luaFilterVariantClass

  it "ignores Divs that don't carry the marker class" $ do
    let doc =
          Pandoc mempty $
            one $
              B.Div
                ("", ["sticky-note"], [])
                [B.CodeBlock B.nullAttr "not an engine error"]
    extractInPlaceFilterErrors doc `shouldBe` []

  it "ignores Divs whose marker is only `emanote:error` (no variant)" $ do
    -- The universal `emanote:error` marker without the `:lua-filter`
    -- variant points at a different diagnostic surface; only the variant
    -- triggers the static-build abort.
    let doc =
          Pandoc mempty $
            one $
              B.Div
                ("", ["emanote:error"], [])
                [B.CodeBlock B.nullAttr "unrelated diagnostic"]
    extractInPlaceFilterErrors doc `shouldBe` []

  it "collects every marker Div in the doc, in order" $ do
    let marker n =
          B.Div
            ("", [luaFilterVariantClass], [])
            [B.CodeBlock B.nullAttr ("engine error " <> n)]
        doc = Pandoc mempty [marker "1", B.Para [B.Str "interlude"], marker "2"]
    extractInPlaceFilterErrors doc `shouldBe` ["engine error 1", "engine error 2"]

  it "walks nested Divs (a marker inside a wrapper)" $ do
    let inner =
          B.Div
            ("", [luaFilterVariantClass], [])
            [B.CodeBlock B.nullAttr "deeply nested"]
        doc = Pandoc mempty $ one $ B.Div B.nullAttr [inner]
    extractInPlaceFilterErrors doc `shouldBe` ["deeply nested"]
