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
import Emanote.Route.ModelRoute (LMLRoute)
import Heist (HeistT)
import Heist.Extra.Splices.Pandoc qualified as Splices
import Heist.Extra.Splices.Pandoc.Ctx qualified as Splices
import Heist.Interpreted qualified as HI
import Relude
import Text.Pandoc.Definition qualified as B

-- | Custom Heist renderer function for specific Pandoc AST nodes
type PandocRenderF astNode n =
  Model ->
  PandocRenderers n ->
  Splices.RenderCtx n ->
  LMLRoute ->
  astNode ->
  Maybe (HI.Splice n)

type PandocInlineRenderer n = PandocRenderF B.Inline n

type PandocBlockRenderer n = PandocRenderF B.Block n

data PandocRenderers n = PandocRenderers
  { pandocInlineRenderers :: [PandocInlineRenderer n],
    pandocBlockRenderers :: [PandocBlockRenderer n]
  }

mkRenderCtxWithPandocRenderers ::
  forall m n.
  (Monad m, Monad n) =>
  PandocRenderers n ->
  Map Text Text ->
  Model ->
  LMLRoute ->
  HeistT n m (Splices.RenderCtx n)
mkRenderCtxWithPandocRenderers nr@PandocRenderers {..} classRules model x =
  Splices.mkRenderCtx
    classRules
    ( \ctx blk ->
        asum $
          pandocBlockRenderers <&> \f ->
            f model nr ctx x blk
    )
    ( \ctx blk ->
        asum $
          pandocInlineRenderers <&> \f ->
            f model nr ctx x blk
    )
