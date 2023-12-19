{-# LANGUAGE RecordWildCards #-}

{- | Types for custom render extensions to Pandoc AST nodes.

 Note that unlike Pandoc *filters* (which operate on entire document), these
 are modeled based on Text.Pandoc.Walk, ie. fine-grained on individual inline
 and block processing. We do this only so as to render a specific node during
 recursion (cf. `rpBlock` and `rpInline` in Render.hs).

 So we expect the extensions to be in Haskell, however external script may be
 supported using a traditional whole-AST extension API.
-}
module Emanote.Pandoc.Renderer (
  PandocRenderers (PandocRenderers),
  PandocInlineRenderer,
  PandocBlockRenderer,
  mkRenderCtxWithPandocRenderers,
  EmanotePandocRenderers (..),
) where

import Heist (HeistT)
import Heist.Extra.Splices.Pandoc qualified as Splices
import Heist.Extra.Splices.Pandoc.Ctx qualified as Splices
import Heist.Interpreted qualified as HI
import Relude
import Text.Pandoc.Definition qualified as B

-- | Custom Heist renderer function for specific Pandoc AST nodes
type PandocRenderF model route astNode =
  model ->
  PandocRenderers model route ->
  Splices.RenderCtx ->
  route ->
  astNode ->
  Maybe (HI.Splice Identity)

type PandocInlineRenderer model route = PandocRenderF model route B.Inline

type PandocBlockRenderer model route = PandocRenderF model route B.Block

data PandocRenderers model route = PandocRenderers
  { pandocInlineRenderers :: [PandocInlineRenderer model route]
  , pandocBlockRenderers :: [PandocBlockRenderer model route]
  }

mkRenderCtxWithPandocRenderers ::
  forall model route m.
  (Monad m) =>
  PandocRenderers model route ->
  Map Text Text ->
  model ->
  route ->
  HeistT Identity m Splices.RenderCtx
mkRenderCtxWithPandocRenderers nr@PandocRenderers {..} classRules model x =
  Splices.mkRenderCtx
    classRules
    ( \ctx blk ->
        asum
          $ pandocBlockRenderers
          <&> \f ->
            f model nr ctx x blk
    )
    ( \ctx blk ->
        asum
          $ pandocInlineRenderers
          <&> \f ->
            f model nr ctx x blk
    )

data EmanotePandocRenderers a r = EmanotePandocRenderers
  { blockRenderers :: PandocRenderers a r
  , inlineRenderers :: PandocRenderers a r
  -- ^ Like `blockRenderers` but for use in inline contexts.
  --
  -- Backlinks and titles constitute an example of inline context, where we don't
  -- care about block elements.
  , linkInlineRenderers :: PandocRenderers a r
  -- ^ Like `inlineRenderers` but suitable for use inside links (<a> tags).
  }
