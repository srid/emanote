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
  EmbedStack,
  emptyEmbedStack,
  startingAt,
  pushEmbedStack,
  embedStackContains,
  embedStackToList,
  EmanotePandocRenderers (..),
) where

import Heist (HeistT)
import Heist.Extra.Splices.Pandoc qualified as Splices
import Heist.Extra.Splices.Pandoc.Ctx qualified as Splices
import Heist.Interpreted qualified as HI
import Relude
import Text.Pandoc.Definition qualified as B

{- | Custom Heist renderer function for specific Pandoc AST nodes.

The 'EmbedStack' carries the chain of routes currently being expanded as note
embeds — the renderer for @![[…]]@ uses it to detect cycles (issue #362).
Renderers that don't participate in note embedding can ignore it.
-}
type PandocRenderF model route astNode =
  model ->
  PandocRenderers model route ->
  EmbedStack route ->
  Splices.RenderCtx ->
  route ->
  astNode ->
  Maybe (HI.Splice Identity)

{- | Chain of routes currently being expanded as note embeds, deepest-first.

Stays opaque so callers must use 'startingAt' or 'emptyEmbedStack' to seed it
— that makes the seeding choice explicit at every call site. The page-render
path picks 'startingAt' so a self-embed (@![[X]]@ inside @X.md@) is caught as
a cycle; an out-of-page caller (e.g. a future preview-pane renderer with no
enclosing page concept) would pass 'emptyEmbedStack' to opt out.
-}
newtype EmbedStack route = EmbedStack [route]
  deriving stock (Eq, Show)

emptyEmbedStack :: EmbedStack route
emptyEmbedStack = EmbedStack []

startingAt :: route -> EmbedStack route
startingAt r = EmbedStack [r]

pushEmbedStack :: route -> EmbedStack route -> EmbedStack route
pushEmbedStack r (EmbedStack rs) = EmbedStack (r : rs)

embedStackContains :: (Eq route) => route -> EmbedStack route -> Bool
embedStackContains r (EmbedStack rs) = r `elem` rs

embedStackToList :: EmbedStack route -> [route]
embedStackToList (EmbedStack rs) = rs

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
  -- | Initial embed-ancestor stack. The page-render path uses 'startingAt'
  -- with the page's own route so a self-embed is caught as a cycle (#362).
  EmbedStack route ->
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
  EmbedStack route ->
  Splices.RenderCtx ->
  route ->
  B.Block ->
  Maybe (HI.Splice Identity)
dispatchBlock model nr stk ctx x blk =
  asum $ pandocBlockRenderers nr <&> \f -> f model nr stk ctx x blk

dispatchInline ::
  model ->
  PandocRenderers model route ->
  EmbedStack route ->
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
