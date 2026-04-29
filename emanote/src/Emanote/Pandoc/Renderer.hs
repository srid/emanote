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

-- | Custom Heist renderer function for specific Pandoc AST nodes.
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
  -- | Rendering feature selection (code highlighting, static math, …)
  Splices.RenderFeatures ->
  HeistT Identity m Splices.RenderCtx
mkRenderCtxWithPandocRenderers nr classRules model x =
  Splices.mkRenderCtx
    classRules
    (\ctx -> dispatchBlock model nr ctx x)
    (\ctx -> dispatchInline model nr ctx x)

{- | Renderer-list dispatcher used both at ctx construction and when a sub-note
descends with an augmented embed-ancestor stack.

Heist-extra's 'Splices.RenderCtx' bakes its splice closures at construction
time and the 'blockSplice' / 'inlineSplice' fields don't take the live ctx as
an argument — so a renderer's view of @ctx@ is whatever was knot-tied in.
'Renderer.Embed' rebuilds the closures (via 'withEmbedStack') with a derived
ctx whenever it needs the renderers' 'HP.getUserData' lookup to see a fresh
embed stack.
-}
dispatchBlock ::
  model ->
  PandocRenderers model route ->
  Splices.RenderCtx ->
  route ->
  B.Block ->
  Maybe (HI.Splice Identity)
dispatchBlock model nr ctx x blk =
  asum $ pandocBlockRenderers nr <&> \f -> f model nr ctx x blk

dispatchInline ::
  model ->
  PandocRenderers model route ->
  Splices.RenderCtx ->
  route ->
  B.Inline ->
  Maybe (HI.Splice Identity)
dispatchInline model nr ctx x inl =
  asum $ pandocInlineRenderers nr <&> \f -> f model nr ctx x inl

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
