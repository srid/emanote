{-# LANGUAGE RecordWildCards #-}

module Heist.Extra.Splices.Pandoc.Ctx
  ( RenderCtx (..),
    mkRenderCtx,
    emptyRenderCtx,
    rewriteClass,
    ctxSansCustomSplicing,
    concatSpliceFunc,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Heist as H
import Heist.Extra.Splices.Pandoc.Attr (concatAttr)
import qualified Heist.Interpreted as HI
import qualified Text.Pandoc.Builder as B
import qualified Text.XmlHtml as X

data RenderCtx n = RenderCtx
  { -- The XML node which contains individual AST rendering definitions
    -- This corresponds to pandoc.tpl
    rootNode :: Maybe X.Node,
    -- Attributes for a given AST node.
    bAttr :: B.Block -> B.Attr,
    iAttr :: B.Inline -> B.Attr,
    -- Class attribute rewrite rules
    classMap :: Map Text Text,
    -- Custom render functions for AST nodes.
    blockSplice :: B.Block -> Maybe (HI.Splice n),
    inlineSplice :: B.Inline -> Maybe (HI.Splice n)
  }

mkRenderCtx ::
  (Monad m, Monad n) =>
  Map Text Text ->
  (RenderCtx n -> B.Block -> Maybe (HI.Splice n)) ->
  (RenderCtx n -> B.Inline -> Maybe (HI.Splice n)) ->
  H.HeistT n m (RenderCtx n)
mkRenderCtx classMap bS iS = do
  node <- H.getParamNode
  pure $
    mkRenderCtxWith
      node
      classMap
      bS
      iS

mkRenderCtxWith ::
  Monad n =>
  X.Node ->
  -- | How to replace classes in Div and Span nodes.
  Map Text Text ->
  -- | Custom handling of AST block nodes
  (RenderCtx n -> B.Block -> Maybe (HI.Splice n)) ->
  -- | Custom handling of AST inline nodes
  (RenderCtx n -> B.Inline -> Maybe (HI.Splice n)) ->
  RenderCtx n
mkRenderCtxWith node classMap bS iS = do
  let ctx =
        RenderCtx
          (Just node)
          (blockLookupAttr node)
          (inlineLookupAttr node)
          classMap
          (bS ctx)
          (iS ctx)
   in ctx

emptyRenderCtx :: RenderCtx n
emptyRenderCtx =
  RenderCtx Nothing (const B.nullAttr) (const B.nullAttr) mempty (const Nothing) (const Nothing)

-- | Strip any custom splicing out of the given render context
ctxSansCustomSplicing :: RenderCtx n -> RenderCtx n
ctxSansCustomSplicing ctx =
  ctx
    { blockSplice = const Nothing,
      inlineSplice = const Nothing
    }

concatSpliceFunc :: Alternative f => (t -> f a) -> (t -> f a) -> t -> f a
concatSpliceFunc f g x =
  asum
    [ f x,
      g x
    ]

rewriteClass :: Monad n => RenderCtx n -> B.Attr -> B.Attr
rewriteClass RenderCtx {..} (id', classes, attr) =
  (id', rewrite classMap <$> classes, attr)
  where
    rewrite :: Ord a => Map a a -> a -> a
    rewrite rules x =
      fromMaybe x $ Map.lookup x rules

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
