{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Heist.Extra.Splices.Pandoc
  ( RenderCtx (..),
    pandocSplice,
    -- | To delegate rendering of blocks and inlines from a custom splice.
    rpBlock,
    rpInline,
    -- Related Heist helpers (for working with pandoc.tpl)
    runCustomNode,
  )
where

import qualified Heist as H
import Heist.Extra.Splices.Pandoc.Ctx
  ( RenderCtx (..),
    mkRenderCtx,
    runCustomNode,
  )
import Heist.Extra.Splices.Pandoc.Footnotes (footnoteRefSplice, gatherFootnotes, renderFootnotesWith)
import Heist.Extra.Splices.Pandoc.Render
  ( renderPandocWith,
    rpBlock,
    rpInline,
  )
import qualified Heist.Interpreted as HI
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))

-- | A splice to render a Pandoc AST allowing customization of the AST nodes in
-- HTML.
pandocSplice ::
  Monad n =>
  -- | How to replace classes in Div and Span nodes.
  Map Text Text ->
  -- | Custom handling of AST block nodes
  (RenderCtx n -> B.Block -> Maybe (HI.Splice n)) ->
  -- | Custom handling of AST inline nodes
  (RenderCtx n -> B.Inline -> Maybe (HI.Splice n)) ->
  Pandoc ->
  HI.Splice n
pandocSplice classMap bS iS doc = do
  node <- H.getParamNode
  -- Render main document, sans footnote content.
  let footnotes = gatherFootnotes doc
      iSWithFootnotes c i =
        asum
          [ iS c i,
            footnoteRefSplice c i
          ]
      ctx =
        mkRenderCtx
          node
          classMap
          bS
          iSWithFootnotes
          footnotes
  docNodes <- renderPandocWith ctx doc
  -- Render footnotes themselves, but without recursing into inner footnotes.
  let footnotesCtx =
        mkRenderCtx
          node
          classMap
          bS
          iS
          []
  footnotesNodes <- renderFootnotesWith footnotesCtx footnotes
  pure $ docNodes <> footnotesNodes
