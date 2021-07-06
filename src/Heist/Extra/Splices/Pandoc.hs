{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Heist.Extra.Splices.Pandoc
  ( RenderCtx (..),
    pandocSplice,
    mkRenderCtxWithoutFootnotes,
    -- | To delegate rendering of blocks and inlines from a custom splice.
    rpBlock,
    rpInline,
  )
where

import qualified Heist as H
import Heist.Extra.Splices.Pandoc.Ctx
  ( RenderCtx (..),
    concatSpliceFunc,
    mkRenderCtx,
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

-- | A splice to render a Pandoc AST
pandocSplice ::
  Monad n =>
  RenderCtx n ->
  Pandoc ->
  HI.Splice n
pandocSplice ctx doc = do
  -- Create a new context to render footnote references
  let docCtx =
        ctx
          { inlineSplice = concatSpliceFunc (inlineSplice ctx) (footnoteRefSplice docCtx),
            footnotes = gatherFootnotes doc
          }
  -- Render main document
  docNodes <- renderPandocWith docCtx doc
  -- Render footnotes themselves, but without recursing into inner footnotes.
  footnotesNodes <- renderFootnotesWith ctx (footnotes docCtx)
  pure $ docNodes <> footnotesNodes

mkRenderCtxWithoutFootnotes ::
  (Monad m, Monad n) =>
  Map Text Text ->
  (RenderCtx n -> B.Block -> Maybe (HI.Splice n)) ->
  (RenderCtx n -> B.Inline -> Maybe (HI.Splice n)) ->
  H.HeistT n m (RenderCtx n)
mkRenderCtxWithoutFootnotes classMap bS iS = do
  node <- H.getParamNode
  pure $
    mkRenderCtx
      node
      classMap
      bS
      iS
      []
