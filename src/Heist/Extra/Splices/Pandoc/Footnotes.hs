module Heist.Extra.Splices.Pandoc.Footnotes where

import qualified Data.List as List
import Heist.Extra.Splices.Pandoc.Ctx (RenderCtx (footnotes))
import Heist.Extra.Splices.Pandoc.Render (rpBlock, rpInline)
import qualified Heist.Interpreted as HI
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))
import qualified Text.Pandoc.Walk as W
import qualified Text.XmlHtml as X

type Footnotes = [[B.Block]]

gatherFootnotes :: Pandoc -> Footnotes
gatherFootnotes = W.query $ \case
  B.Note footnote ->
    [footnote]
  _ ->
    []

lookupFootnote :: HasCallStack => [B.Block] -> Footnotes -> Int
lookupFootnote note fs =
  fromMaybe (error $ "Missing footnote: " <> show note) $ do
    (+ 1) <$> List.elemIndex note fs

renderFootnotesWith :: forall n. Monad n => RenderCtx n -> Footnotes -> HI.Splice n
renderFootnotesWith ctx = \case
  [] -> pure []
  fs -> do
    let footnotesWithIdx = zip [1 :: Int ..] fs
    -- TODO: Allow styling pandoc.tpl
    fmap (one . X.Element "ol" [("title", "Footnotes"), ("class", "list-decimal list-inside space-y-1 mt-4 pt-2 pl-2 border-t-2 text-gray-700")]) $
      flip foldMapM footnotesWithIdx $
        fmap (one . X.Element "li" mempty) . uncurry rpFootnote
  where
    -- TODO: Use index to create hyperlinks
    rpFootnote :: Int -> [B.Block] -> HI.Splice n
    rpFootnote _idx bs =
      one . X.Element "div" [("class", "inline-block")] <$> case bs of
        [B.Para is] ->
          -- Optimize for the most usual case, by discarding the paragraph,
          -- which adds unnecessary styling (thus margins).
          foldMapM (rpInline ctx) is
        _ ->
          foldMapM (rpBlock ctx) bs

footnoteRefSplice :: Monad n => RenderCtx n -> B.Inline -> Maybe (HI.Splice n)
footnoteRefSplice ctx = \case
  B.Note bs -> pure $ do
    -- TODO: Allow styling via pandoc.tpl
    let idx = lookupFootnote bs $ footnotes ctx
    pure $
      one $
        X.Element "sup" [("class", "px-0.5")] $ one . X.TextNode $ show idx
  _ ->
    Nothing
