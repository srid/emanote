{-# LANGUAGE RecordWildCards #-}

-- | Types for custom render extensions to Pandoc AST nodes.
--
-- Note that unlike Pandoc *filters* (which operate on entire document), these
-- are modeled based on Text.Pandoc.Walk, ie. fine-grained on individual inline
-- and block processing. We do this only so as to render a specific node during
-- recursion (cf. `rpBlock` and `rpInline` in Render.hs).
--
-- So we expect the extensions to be in Haskell, however external script may be
-- supported using a traditional whole-AST extension API.
module Emanote.Pandoc.Renderer
  ( PandocRenderers (PandocRenderers),
    PandocInlineRenderer,
    PandocBlockRenderer,
    mkRenderCtxWithPandocRenderers,
  )
where

import Emanote.Model.Type (Model)
import Heist (HeistT)
import Heist.Extra.Splices.Pandoc qualified as Splices
import Heist.Extra.Splices.Pandoc.Ctx qualified as Splices
import Heist.Interpreted qualified as HI
import Relude
import Text.Pandoc.Definition qualified as B

-- | Custom Heist renderer function for specific Pandoc AST nodes
--
-- The `x` selects between `i` (inline) and `b` (block) types.
type PandocRenderF astType n i b x =
  Model ->
  PandocRenderers n i b ->
  Splices.RenderCtx n ->
  x ->
  astType ->
  Maybe (HI.Splice n)

type PandocInlineRenderer n i b = PandocRenderF B.Inline n i b i

type PandocBlockRenderer n i b = PandocRenderF B.Block n i b b

data PandocRenderers n i b = PandocRenderers
  { pandocInlineRenderers :: [PandocInlineRenderer n i b],
    pandocBlockRenderers :: [PandocBlockRenderer n i b]
  }

mkRenderCtxWithPandocRenderers ::
  forall i b m n.
  (Monad m, Monad n) =>
  PandocRenderers n i b ->
  Map Text Text ->
  Model ->
  i ->
  b ->
  HeistT n m (Splices.RenderCtx n)
mkRenderCtxWithPandocRenderers nr@PandocRenderers {..} classRules model i b =
  Splices.mkRenderCtx
    classRules
    ( \ctx blk ->
        asum $
          pandocBlockRenderers <&> \f ->
            f model nr ctx b blk
    )
    ( \ctx blk ->
        asum $
          pandocInlineRenderers <&> \f ->
            f model nr ctx i blk
    )
