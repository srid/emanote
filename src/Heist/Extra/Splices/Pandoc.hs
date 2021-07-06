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
  )
where

import Heist.Extra.Splices.Pandoc.Ctx
  ( RenderCtx (..),
    concatSpliceFunc,
  )
import Heist.Extra.Splices.Pandoc.Footnotes
  ( footnoteRefSplice,
    gatherFootnotes,
    renderFootnotesWith,
  )
import Heist.Extra.Splices.Pandoc.Render
  ( renderPandocWith,
    rpBlock,
    rpInline,
  )
import qualified Heist.Interpreted as HI
import Text.Pandoc.Definition (Pandoc (..))

-- | A splice to render a Pandoc AST
pandocSplice ::
  Monad n =>
  RenderCtx n ->
  Pandoc ->
  HI.Splice n
pandocSplice ctx doc = do
  -- Create a new context to render footnote references
  let footnotes = gatherFootnotes doc
      docCtx =
        ctx
          { inlineSplice = concatSpliceFunc (inlineSplice ctx) (footnoteRefSplice docCtx footnotes)
          }
  -- Render main document
  docNodes <- renderPandocWith docCtx doc
  -- Render footnotes themselves, but without recursing into inner footnotes.
  footnotesNodes <- renderFootnotesWith ctx footnotes
  pure $ docNodes <> footnotesNodes
