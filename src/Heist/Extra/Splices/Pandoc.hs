{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Heist.Extra.Splices.Pandoc
  ( RenderCtx (..),
    pandocSplice,
    pandocSpliceWithCustomClass,
    -- | To delegate rendering of blocks and inlines from a custom splice.
    rpBlock,
    rpInline,
    rpBlock',
    rpInline',
  )
where

import qualified Heist as H
import Heist.Extra.Splices.Pandoc.Ctx
  ( RenderCtx (..),
    mkRenderCtx,
  )
import Heist.Extra.Splices.Pandoc.Footnotes (gatherFootnotes, renderFootnotesWith)
import Heist.Extra.Splices.Pandoc.Render
import qualified Heist.Interpreted as HI
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))

pandocSplice :: Monad n => Pandoc -> HI.Splice n
pandocSplice =
  pandocSpliceWithCustomClass
    mempty
    (const . const $ Nothing)
    (const . const $ Nothing)

-- | A splice to render a Pandoc AST allowing customization of the AST nodes in
-- HTML.
pandocSpliceWithCustomClass ::
  Monad n =>
  -- | How to replace classes in Div and Span nodes.
  Map Text Text ->
  -- | Custom handling of AST block nodes
  (RenderCtx n -> B.Block -> Maybe (HI.Splice n)) ->
  -- | Custom handling of AST inline nodes
  (RenderCtx n -> B.Inline -> Maybe (HI.Splice n)) ->
  Pandoc ->
  HI.Splice n
pandocSpliceWithCustomClass classMap bS iS doc = do
  node <- H.getParamNode
  let ctx =
        mkRenderCtx
          node
          classMap
          bS
          iS
          (gatherFootnotes doc)
  docNodes <- renderPandocWith ctx doc
  footnotesNodes <- renderFootnotesWith (ctx {footnotes = []}) $ footnotes ctx
  pure $ docNodes <> footnotesNodes
