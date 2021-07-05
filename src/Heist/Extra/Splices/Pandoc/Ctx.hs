{-# LANGUAGE RecordWildCards #-}

module Heist.Extra.Splices.Pandoc.Ctx where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Heist.Extra.Splices.Pandoc.Attr (concatAttr)
import qualified Heist.Interpreted as HI
import qualified Text.Pandoc.Builder as B
import qualified Text.XmlHtml as X

data RenderCtx n = RenderCtx
  { rootNode :: X.Node,
    -- Attributes for a given AST node.
    bAttr :: B.Block -> B.Attr,
    iAttr :: B.Inline -> B.Attr,
    -- Class attribute rewrite rules
    classMap :: Map Text Text,
    -- Custom render functions for AST nodes.
    blockSplice :: B.Block -> Maybe (HI.Splice n),
    inlineSplice :: B.Inline -> Maybe (HI.Splice n),
    -- Footnotes gathered in advance
    footnotes :: [[B.Block]]
  }

mkRenderCtx ::
  Monad n =>
  X.Node ->
  -- | How to replace classes in Div and Span nodes.
  Map Text Text ->
  -- | Custom handling of AST block nodes
  (RenderCtx n -> B.Block -> Maybe (HI.Splice n)) ->
  -- | Custom handling of AST inline nodes
  (RenderCtx n -> B.Inline -> Maybe (HI.Splice n)) ->
  -- | Footnotes gathered ahead
  [[B.Block]] ->
  RenderCtx n
mkRenderCtx node classMap bS iS footnotes = do
  let ctx =
        RenderCtx
          node
          (blockLookupAttr node)
          (inlineLookupAttr node)
          classMap
          (bS ctx)
          (iS ctx)
          footnotes
   in ctx

-- | Strip any custom splicing out of the given render context
ctxSansCustomSplicing :: RenderCtx n -> RenderCtx n
ctxSansCustomSplicing ctx =
  ctx
    { blockSplice = const Nothing,
      inlineSplice = const Nothing
    }

rewriteClass :: Monad n => RenderCtx n -> B.Attr -> B.Attr
rewriteClass RenderCtx {..} (id', classes, attr) =
  let x =
        classes <&> \cls ->
          fromMaybe cls $ Map.lookup cls classMap
   in (id', x, attr)

blockLookupAttr :: X.Node -> B.Block -> B.Attr
blockLookupAttr node = \case
  B.Para {} -> childTagAttr node "Para"
  B.BulletList {} -> childTagAttr node "BulletList"
  B.OrderedList {} -> childTagAttr node "OrderedList"
  B.CodeBlock {} -> childTagAttr node "CodeBlock"
  B.BlockQuote {} -> childTagAttr node "BlockQuote"
  B.Header level _ _ ->
    fromMaybe B.nullAttr $ do
      header <- X.childElementTag "Header" node
      pure $ childTagAttr header ("h" <> show level)
  _ -> B.nullAttr

inlineLookupAttr :: X.Node -> B.Inline -> B.Attr
inlineLookupAttr node = \case
  B.Code {} -> childTagAttr node "Code"
  B.Note _ ->
    childTagAttr node "Note"
  B.Link _ _ (url, _) ->
    fromMaybe B.nullAttr $ do
      link <- X.childElementTag "PandocLink" node
      let innerTag = if "://" `T.isInfixOf` url then "External" else "Internal"
      pure $ attrFromNode link `concatAttr` childTagAttr link innerTag
  _ -> B.nullAttr

childTagAttr :: X.Node -> Text -> B.Attr
childTagAttr x name =
  maybe B.nullAttr attrFromNode $ X.childElementTag name x

attrFromNode :: X.Node -> B.Attr
attrFromNode node =
  let mClass = maybe mempty T.words $ X.getAttribute "class" node
      id' = fromMaybe "" $ X.getAttribute "id" node
      attrs = filter ((/= "class") . fst) $ X.elementAttrs node
   in (id', mClass, attrs)
