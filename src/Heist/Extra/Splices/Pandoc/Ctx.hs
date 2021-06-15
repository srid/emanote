{-# LANGUAGE RecordWildCards #-}

module Heist.Extra.Splices.Pandoc.Ctx where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Heist as H
import Heist.Extra.Splices.Pandoc.Attr (addAttr)
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
rewriteClass RenderCtx {..} (id', cls, attr) =
  let cls' = maybe cls T.words $ Map.lookup (T.intercalate " " cls) classMap
   in (id', cls', attr)

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
      pure $ attrFromNode link `addAttr` childTagAttr link innerTag
  _ -> B.nullAttr

-- | Useful for running a splice against an arbitrary node (such as that pulled from pandoc.tpl)
runCustomNode :: Monad n => X.Node -> H.Splices (HI.Splice n) -> HI.Splice n
runCustomNode node splices =
  H.localHS (HI.bindSplices splices) $ do
    HI.runNode node <&> \case
      [resNode]
        | X.elementTag resNode == X.elementTag node ->
          -- Get rid of the `node` itself.
          X.elementChildren resNode
      res ->
        res

childTagAttr :: X.Node -> Text -> B.Attr
childTagAttr x name =
  maybe B.nullAttr attrFromNode $ X.childElementTag name x

attrFromNode :: X.Node -> B.Attr
attrFromNode node =
  let mClass = maybe mempty T.words $ X.getAttribute "class" node
      id' = fromMaybe "" $ X.getAttribute "id" node
      attrs = filter ((/= "class") . fst) $ X.elementAttrs node
   in (id', mClass, attrs)
