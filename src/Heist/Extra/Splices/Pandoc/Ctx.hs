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

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Heist qualified as H
import Heist.Extra.Splices.Pandoc.Attr (concatAttr)
import Heist.Interpreted qualified as HI
import Relude
import Text.Pandoc.Builder qualified as B
import Text.Pandoc.Walk qualified as W
import Text.XmlHtml qualified as X

-- | The configuration context under which we must render a `Pandoc` document
-- using the given Heist template.
data RenderCtx = RenderCtx
  { -- The XML node which contains individual AST rendering definitions
    -- This corresponds to pandoc.tpl
    rootNode :: Maybe X.Node,
    -- Attributes for a given AST node.
    bAttr :: B.Block -> B.Attr,
    iAttr :: B.Inline -> B.Attr,
    -- Class attribute rewrite rules
    classMap :: Map Text Text,
    -- Custom render functions for AST nodes.
    blockSplice :: B.Block -> Maybe (HI.Splice Identity),
    inlineSplice :: B.Inline -> Maybe (HI.Splice Identity)
  }

mkRenderCtx ::
  (Monad m) =>
  Map Text Text ->
  (RenderCtx -> B.Block -> Maybe (HI.Splice Identity)) ->
  (RenderCtx -> B.Inline -> Maybe (HI.Splice Identity)) ->
  H.HeistT Identity m RenderCtx
mkRenderCtx classMap bS iS = do
  node <- H.getParamNode
  pure $
    mkRenderCtxWith
      node
      classMap
      bS
      iS

mkRenderCtxWith ::
  X.Node ->
  -- | How to replace classes in Div and Span nodes.
  Map Text Text ->
  -- | Custom handling of AST block nodes
  (RenderCtx -> B.Block -> Maybe (HI.Splice Identity)) ->
  -- | Custom handling of AST inline nodes
  (RenderCtx -> B.Inline -> Maybe (HI.Splice Identity)) ->
  RenderCtx
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

emptyRenderCtx :: RenderCtx
emptyRenderCtx =
  RenderCtx Nothing (const B.nullAttr) (const B.nullAttr) mempty (const Nothing) (const Nothing)

-- | Strip any custom splicing out of the given render context
ctxSansCustomSplicing :: RenderCtx -> RenderCtx
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

rewriteClass :: RenderCtx -> B.Attr -> B.Attr
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
  B.Link _ inlines (url, _) ->
    let isExternal = "://" `T.isInfixOf` url
        disableIcon =
          getAny $
            W.query
              ( \case
                  B.Image {} -> Any True
                  B.Link {} -> Any True -- this handles wikilinks
                  _ -> Any False
              )
              inlines
     in fromMaybe B.nullAttr $ do
          linkNode <- X.childElementTag "PandocLink" node
          let linkTypeNode =
                X.childElementTag
                  ( if isExternal
                      then "External"
                      else "Internal"
                  )
                  linkNode
          let noIconNode =
                if isExternal && disableIcon
                  then linkTypeNode >>= X.childElementTag "NoIcon"
                  else Nothing
          pure $
            attrFromNode linkNode
              `concatAttr` attrFromMaybeNode linkTypeNode
              `concatAttr` attrFromMaybeNode noIconNode
  _ -> B.nullAttr

attrFromMaybeNode :: Maybe X.Node -> B.Attr
attrFromMaybeNode = maybe B.nullAttr attrFromNode

childTagAttr :: X.Node -> Text -> B.Attr
childTagAttr x name =
  attrFromMaybeNode $ X.childElementTag name x

attrFromNode :: X.Node -> B.Attr
attrFromNode node =
  let mClass = maybe mempty words $ X.getAttribute "class" node
      id' = fromMaybe "" $ X.getAttribute "id" node
      attrs = filter ((/= "class") . fst) $ X.elementAttrs node
   in (id', mClass, attrs)
