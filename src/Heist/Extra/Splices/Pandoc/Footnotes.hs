module Heist.Extra.Splices.Pandoc.Footnotes where

import Data.List qualified as List
import Data.Map.Syntax ((##))
import Heist qualified as H
import Heist.Extra (runCustomNode)
import Heist.Extra.Splices.Pandoc.Ctx (RenderCtx (rootNode))
import Heist.Extra.Splices.Pandoc.Render (renderPandocWith)
import Heist.Interpreted qualified as HI
import Relude
import Text.Pandoc.Builder qualified as B
import Text.Pandoc.Definition (Pandoc (..))
import Text.Pandoc.Walk qualified as W
import Text.XmlHtml qualified as X

type Footnotes = [[B.Block]]

gatherFootnotes :: Pandoc -> Footnotes
gatherFootnotes = List.nub . W.query queryFootnotes
  where
    queryFootnotes = \case
      B.Note footnote ->
        [footnote]
      _ ->
        []

lookupFootnote :: HasCallStack => [B.Block] -> Footnotes -> Int
lookupFootnote note fs =
  fromMaybe (error $ "Missing footnote: " <> show note) $ do
    (+ 1) <$> List.elemIndex note fs

renderFootnotesWith :: forall n. Monad n => RenderCtx n -> Footnotes -> HI.Splice n
renderFootnotesWith ctx fs' =
  fromMaybe (pure []) $ do
    fs <- toList <$> nonEmpty fs'
    renderNode <- fmap head . nonEmpty $ maybe [] (X.childElementsTag "Note:List") $ rootNode ctx
    let footnotesWithIdx = zip [1 :: Int ..] fs
    Just $
      runCustomNode renderNode $ do
        "footnote"
          ## (HI.runChildrenWith . uncurry (footnoteSplices ctx)) `foldMapM` footnotesWithIdx

footnoteSplices :: Monad n => RenderCtx n -> Int -> [B.Block] -> H.Splices (HI.Splice n)
footnoteSplices ctx idx bs = do
  let footnoteDoc = Pandoc mempty $ case bs of
        [B.Para is] ->
          -- Optimize for the most usual case, by discarding the paragraph,
          -- which adds unnecessary styling (thus margins).
          one $ B.Plain is
        _ ->
          bs
  "footnote:idx" ## HI.textSplice (show idx)
  "footnote:content" ## renderPandocWith ctx footnoteDoc

footnoteRefSplice :: Monad n => RenderCtx n -> [[B.Block]] -> B.Inline -> Maybe (HI.Splice n)
footnoteRefSplice ctx footnotes inline = do
  B.Note bs <- pure inline
  let idx = lookupFootnote bs footnotes
  renderNode <- fmap head . nonEmpty $ maybe [] (X.childElementsTag "Note:Ref") (rootNode ctx)
  Just $
    runCustomNode renderNode $
      footnoteSplices ctx idx bs
