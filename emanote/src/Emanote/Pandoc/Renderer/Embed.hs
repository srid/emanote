{- | Resolving renderers for note embedding (@![[note]]@).

Cycle handling: a note that embeds itself, or two notes that embed each
other, used to expand the embed without a fixpoint (issue #362). The
'EmbedStack' is the chain of routes currently being expanded; the embed
renderer reads it from the ctx's typed user-data slot via 'currentEmbedStack'
and writes the augmented chain via 'setUserData' before recursing. When the
target route is already on the stack, 'renderCyclicEmbedSplice' produces an
inline placeholder instead of recursing.

The stack is stored in 'HP.RenderCtx''s @userData@ slot rather than threaded
through 'PandocRenderF', so URL / callout / query renderers — which don't
participate in note embedding — don't have to mention the embed-only concern
in their signatures.
-}
module Emanote.Pandoc.Renderer.Embed where

import Commonmark.Extensions.WikiLink qualified as WL
import Data.Map.Syntax ((##))
import Emanote.Model (Model)
import Emanote.Model qualified as M
import Emanote.Model.Link.Rel qualified as Rel
import Emanote.Model.Link.Resolve qualified as Resolve
import Emanote.Model.Note qualified as MN
import Emanote.Model.StaticFile (CodeLanguage (..), StaticFileInfo (..), staticFileInfoTemplateName)
import Emanote.Model.StaticFile qualified as SF
import Emanote.Model.Title qualified as Tit
import Emanote.Model.Toc (newToc, renderToc)
import Emanote.Pandoc.Link qualified as Link
import Emanote.Pandoc.Renderer (PandocBlockRenderer, PandocInlineRenderer, PandocRenderers, dispatchBlock, dispatchInline)
import Emanote.Pandoc.Renderer.Url qualified as RendererUrl
import Emanote.Route.ModelRoute qualified as R
import Emanote.Route.SiteRoute qualified as SF
import Emanote.Route.SiteRoute qualified as SR
import Heist qualified as H
import Heist.Extra qualified as HE
import Heist.Extra.Splices.Pandoc (pandocSplice, rpBlock)
import Heist.Extra.Splices.Pandoc qualified as HP
import Heist.Extra.Splices.Pandoc.Ctx (getUserData, setUserData)
import Heist.Interpreted qualified as HI
import Optics.Operators ((^.))
import Relude
import Text.Pandoc.Definition qualified as B

{- | Chain of routes currently being expanded as note embeds, deepest-first.

Stays opaque so 'View.Common' must use 'startingAt' (or 'emptyEmbedStack'
for an out-of-page caller) to seed it — that makes the seeding choice
explicit at every call site. The page-render path picks 'startingAt' so a
self-embed (@![[X]]@ inside @X.md@) is caught as a cycle.
-}
newtype EmbedStack = EmbedStack [R.LMLRoute]
  deriving stock (Eq, Show)
  deriving newtype (Typeable)

emptyEmbedStack :: EmbedStack
emptyEmbedStack = EmbedStack []

startingAt :: R.LMLRoute -> EmbedStack
startingAt r = EmbedStack [r]

pushEmbedStack :: R.LMLRoute -> EmbedStack -> EmbedStack
pushEmbedStack r (EmbedStack rs) = EmbedStack (r : rs)

embedStackContains :: R.LMLRoute -> EmbedStack -> Bool
embedStackContains r (EmbedStack rs) = r `elem` rs

embedStackToList :: EmbedStack -> [R.LMLRoute]
embedStackToList (EmbedStack rs) = rs

-- | Read the current embed stack from a ctx; missing slot is treated as empty.
currentEmbedStack :: HP.RenderCtx -> EmbedStack
currentEmbedStack = fromMaybe emptyEmbedStack . getUserData

embedBlockWikiLinkResolvingSplice :: PandocBlockRenderer Model R.LMLRoute
embedBlockWikiLinkResolvingSplice model nr ctx noteRoute node = do
  B.Para [inl] <- pure node
  (inlRef, (_, _, otherAttrs), is, (url, tit)) <- Link.parseInlineRef inl
  guard $ inlRef == Link.InlineLink
  let parentR = M.modelResolveLinkBase model noteRoute
  -- TODO: Use anchor to embed a section?
  (Rel.URTWikiLink (WL.WikiLinkEmbed, wl), _mAnchor) <-
    Rel.parseUnresolvedRelTarget parentR (otherAttrs <> one ("title", tit)) url
  let rRel = Resolve.resolveWikiLinkMustExist model noteRoute wl
  RendererUrl.renderSomeInlineRefWith Resolve.resourceSiteRoute (is, (url, tit)) rRel model ctx inl $ \case
    Left (R.LMLView_Html, r) -> embedResourceRoute model nr ctx noteRoute r
    Right sf
      | isJust (SF._staticFileInfo sf) ->
          embedStaticFileRoute model (toText $ SF._staticFilePath sf) sf
    _ -> Nothing

embedBlockRegularLinkResolvingSplice :: PandocBlockRenderer Model R.LMLRoute
embedBlockRegularLinkResolvingSplice model _nr ctx noteRoute node = do
  B.Para [inl] <- pure node
  (inlRef, (_, _, otherAttrs), is, (url, tit)) <- Link.parseInlineRef inl
  guard $ inlRef == Link.InlineImage
  let parentR = M.modelResolveLinkBase model noteRoute
  (Rel.URTResource candidates, _mAnchor) <-
    Rel.parseUnresolvedRelTarget parentR (otherAttrs <> one ("title", tit)) url
  let rRel = Resolve.resolveModelRouteCandidates model candidates
  RendererUrl.renderSomeInlineRefWith Resolve.resourceSiteRoute (is, (url, tit)) rRel model ctx inl
    $ either (const Nothing) (embedStaticFileRoute model $ WL.plainify is)

embedInlineWikiLinkResolvingSplice :: PandocInlineRenderer Model R.LMLRoute
embedInlineWikiLinkResolvingSplice model _nr ctx noteRoute inl = do
  (inlRef, (_, _, otherAttrs), is, (url, tit)) <- Link.parseInlineRef inl
  guard $ inlRef == Link.InlineLink
  let parentR = M.modelResolveLinkBase model noteRoute
  (Rel.URTWikiLink (WL.WikiLinkEmbed, wl), _mAnchor) <- Rel.parseUnresolvedRelTarget parentR (otherAttrs <> one ("title", tit)) url
  let rRel = Resolve.resolveWikiLinkMustExist model noteRoute wl
  RendererUrl.renderSomeInlineRefWith Resolve.resourceSiteRoute (is, (url, tit)) rRel model ctx inl
    $ either (const Nothing) (embedStaticFileRoute model $ show wl)

runEmbedTemplate :: ByteString -> H.Splices (HI.Splice Identity) -> HI.Splice Identity
runEmbedTemplate name splices = do
  tpl <- HE.lookupHtmlTemplateMust $ "/templates/filters/embed-" <> name
  HE.runCustomTemplate tpl splices

{- | Render an embedded note (@![[note]]@) as inlined Pandoc.

Reads the embed-ancestor stack from 'ctx's user-data slot and short-circuits
to 'renderCyclicEmbedSplice' when the target is already on the chain — that
breaks what would otherwise be an infinite expansion (issue #362). Otherwise
the target route is pushed onto the stack and 'withEmbedStack' rebuilds the
ctx so the renderers fired from the embedded body see the augmented chain.
-}
embedResourceRoute ::
  Model ->
  PandocRenderers Model R.LMLRoute ->
  HP.RenderCtx ->
  -- | Route used as the relative-link base inside the embedded body. See
  -- the @withEmbedStack@ note about pre-existing depth-1 link-base behaviour.
  R.LMLRoute ->
  MN.Note ->
  Maybe (HI.Splice Identity)
embedResourceRoute model nr ctx embedderRoute note = do
  let embedStack = currentEmbedStack ctx
      targetRoute = note ^. MN.noteRoute
  if embedStackContains targetRoute embedStack
    then pure $ renderCyclicEmbedSplice model ctx embedStack note
    else do
      let nestedCtx = withEmbedStack nr model embedderRoute (pushEmbedStack targetRoute embedStack) ctx
      pure . runEmbedTemplate "note" $ do
        "ema:note:title" ## Tit.titleSplice nestedCtx id (MN._noteTitle note)
        "ema:note:url" ## HI.textSplice (SR.siteRouteUrl model $ SR.lmlSiteRoute (R.LMLView_Html, note ^. MN.noteRoute))
        "ema:note:pandoc" ##
          pandocSplice nestedCtx (note ^. MN.noteDoc)
        "ema:note:toc" ##
          renderToc nestedCtx (newToc $ note ^. MN.noteDoc)

{- | Rebuild a 'HP.RenderCtx' with the new embed stack baked into both
'HP.userData' (so the embed renderer can read it via 'getUserData') *and*
the 'HP.blockSplice' / 'HP.inlineSplice' closures (so renderers fired from
the embedded body see this updated ctx, not the construction-time one).

The closure rebuild ties the knot — @newCtx@ appears on both the left and
inside its own field assignments. This is well-defined as long as
'HP.blockSplice' / 'HP.inlineSplice' stay non-strict fields in
'HP.RenderCtx' — at construction time they're stored as thunks closing over
@newCtx@, and forcing them later finds the fully evaluated record.
-}
withEmbedStack ::
  PandocRenderers Model R.LMLRoute ->
  Model ->
  R.LMLRoute ->
  EmbedStack ->
  HP.RenderCtx ->
  HP.RenderCtx
withEmbedStack nr model x newStack origCtx =
  let newCtx =
        (setUserData newStack origCtx)
          { HP.blockSplice = dispatchBlock model nr newCtx x
          , HP.inlineSplice = dispatchInline model nr newCtx x
          }
   in newCtx

{- | Inline placeholder shown in place of an embed that would form a cycle.

Surfaces the offending note's title plus the chain of ancestor embeds in
deepest-first order, so a reader can see exactly which path closed the loop.
-}
renderCyclicEmbedSplice :: Model -> HP.RenderCtx -> EmbedStack -> MN.Note -> HI.Splice Identity
renderCyclicEmbedSplice model ctx embedStack note =
  rpBlock ctx
    $ B.Div ("", ["emanote:error:cyclic-embed"], [])
    $ one
    $ B.Para
    $ [ B.Strong [B.Str "↺", B.Space, B.Str "Cyclic embed:"]
      , B.Space
      , B.Code B.nullAttr (Tit.toPlain (note ^. MN.noteTitle))
      ]
    <> chainSuffix
  where
    chain = embedStackToList embedStack
    chainSuffix
      | null chain = []
      | otherwise =
          [B.Space, B.Str "(via", B.Space]
            <> intercalate [B.Str ",", B.Space] (pure . renderRoute <$> chain)
            <> [B.Str ")"]
    renderRoute :: R.LMLRoute -> B.Inline
    renderRoute r = B.Code B.nullAttr (Tit.toPlain (M.modelLookupTitle r model))

embedStaticFileRoute :: Model -> Text -> SF.StaticFile -> Maybe (HI.Splice Identity)
embedStaticFileRoute model altText staticFile = do
  let url = SF.siteRouteUrl model $ SF.staticFileSiteRoute staticFile
  staticFileInfo <- SF._staticFileInfo staticFile
  pure . runEmbedTemplate (staticFileInfoTemplateName staticFileInfo) $ do
    case staticFileInfo of
      StaticFileInfoImage -> do
        "ema:url" ## HI.textSplice url
        "ema:alt" ## HI.textSplice altText
      StaticFileInfoVideo ->
        "ema:url" ## HI.textSplice url
      StaticFileInfoAudio ->
        "ema:url" ## HI.textSplice url
      StaticFileInfoPDF ->
        "ema:url" ## HI.textSplice url
      StaticFileInfoCode (CodeLanguage language) content -> do
        "ema:code:content" ## HI.textSplice content
        "ema:code:language" ## HI.textSplice language
        "ema:alt" ## HI.textSplice altText
