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
  dispatchBlock,
  dispatchInline,
  EmanotePandocRenderers (..),
) where

import Heist (HeistT)
import Heist.Extra.Splices.Pandoc qualified as Splices
import Heist.Extra.Splices.Pandoc.Ctx qualified as Splices
import Heist.Interpreted qualified as HI
import Relude
import Text.Pandoc.Definition qualified as B

{- | Custom Heist renderer function for specific Pandoc AST nodes.

The @[route]@ is the chain of routes currently being expanded as note embeds,
deepest-first — the renderer for @![[…]]@ uses 'elem' to detect cycles
(issue #362) and the chain is also surfaced in the placeholder shown when a
cycle is hit. Renderers that don't participate in note embedding can ignore
it.
-}
type PandocRenderF model route astNode =
  model ->
  PandocRenderers model route ->
  [route] ->
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
  -- | Initial embed-ancestor stack. At top level this is the page's own route,
  -- so that a page directly embedding itself is caught.
  [route] ->
  Map Text Text ->
  model ->
  route ->
  -- | Rendering feature selection (code highlighting, static math, …)
  Splices.RenderFeatures ->
  HeistT Identity m Splices.RenderCtx
mkRenderCtxWithPandocRenderers nr embedStack classRules model x =
  Splices.mkRenderCtx
    classRules
    (\ctx -> dispatchBlock model nr embedStack ctx x)
    (\ctx -> dispatchInline model nr embedStack ctx x)

-- The dispatchers take their arguments in the same order as 'PandocRenderF'
-- so the renderer call inside is a one-to-one positional pass-through.
-- Embed.hs uses them to rebuild a ctx with an augmented embed-ancestor stack
-- when descending into a sub-note (see 'withEmbedStack').
dispatchBlock ::
  model ->
  PandocRenderers model route ->
  [route] ->
  Splices.RenderCtx ->
  route ->
  B.Block ->
  Maybe (HI.Splice Identity)
dispatchBlock model nr stk ctx x blk =
  asum $ pandocBlockRenderers nr <&> \f -> f model nr stk ctx x blk

dispatchInline ::
  model ->
  PandocRenderers model route ->
  [route] ->
  Splices.RenderCtx ->
  route ->
  B.Inline ->
  Maybe (HI.Splice Identity)
dispatchInline model nr stk ctx x inl =
  asum $ pandocInlineRenderers nr <&> \f -> f model nr stk ctx x inl

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
