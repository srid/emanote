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
import Emanote.Pandoc.Renderer (
  EmbedStack,
  PandocBlockRenderer,
  PandocInlineRenderer,
  PandocRenderers,
  dispatchBlock,
  dispatchInline,
  embedStackContains,
  embedStackToList,
  pushEmbedStack,
 )
import Emanote.Pandoc.Renderer.Url qualified as RendererUrl
import Emanote.Route.ModelRoute qualified as R
import Emanote.Route.SiteRoute qualified as SF
import Emanote.Route.SiteRoute qualified as SR
import Heist qualified as H
import Heist.Extra qualified as HE
import Heist.Extra.Splices.Pandoc (pandocSplice, rpBlock)
import Heist.Extra.Splices.Pandoc qualified as HP
import Heist.Interpreted qualified as HI
import Optics.Operators ((^.))
import Relude
import Text.Pandoc.Definition qualified as B

embedBlockWikiLinkResolvingSplice :: PandocBlockRenderer Model R.LMLRoute
embedBlockWikiLinkResolvingSplice model nr embedStack ctx noteRoute node = do
  B.Para [inl] <- pure node
  (inlRef, (_, _, otherAttrs), is, (url, tit)) <- Link.parseInlineRef inl
  guard $ inlRef == Link.InlineLink
  let parentR = M.modelResolveLinkBase model noteRoute
  -- TODO: Use anchor to embed a section?
  (Rel.URTWikiLink (WL.WikiLinkEmbed, wl), _mAnchor) <-
    Rel.parseUnresolvedRelTarget parentR (otherAttrs <> one ("title", tit)) url
  let rRel = Resolve.resolveWikiLinkMustExist model noteRoute wl
  RendererUrl.renderSomeInlineRefWith Resolve.resourceSiteRoute (is, (url, tit)) rRel model ctx inl $ \case
    -- Pass `noteRoute` as the embedder route, preserving the pre-existing
    -- behaviour that relative wikilinks inside an embedded note are resolved
    -- against the embedder, not against the embedded note. Whether that's
    -- correct at depth >1 is a separate question (orthogonal to #362).
    Left (R.LMLView_Html, r) -> embedResourceRoute model nr embedStack ctx noteRoute r
    Right sf
      | isJust (SF._staticFileInfo sf) ->
          embedStaticFileRoute model (toText $ SF._staticFilePath sf) sf
    _ -> Nothing

embedBlockRegularLinkResolvingSplice :: PandocBlockRenderer Model R.LMLRoute
embedBlockRegularLinkResolvingSplice model _nr _embedStack ctx noteRoute node = do
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
embedInlineWikiLinkResolvingSplice model _nr _embedStack ctx noteRoute inl = do
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

Cycle handling: if the target note is already an ancestor in 'embedStack',
render an inline error placeholder instead of recursing — otherwise the embed
would expand without a fixpoint and either hang the renderer or produce
arbitrarily deep nested output (issue #362).

When recursion is safe, the splice closures inside 'ctx' are rebuilt to carry
the target route prepended to 'embedStack', so any nested embed inside this
note sees up-to-date ancestry.
-}
embedResourceRoute ::
  Model ->
  PandocRenderers Model R.LMLRoute ->
  EmbedStack R.LMLRoute ->
  HP.RenderCtx ->
  -- | Route of the note that *contains* this embed (the embedder). Threaded
  -- into the nested splice closures unchanged, preserving the pre-existing
  -- behaviour that relative links inside the embedded note resolve against
  -- the embedder. Not the same as @note ^. MN.noteRoute@ (the embedded
  -- note's own route).
  R.LMLRoute ->
  MN.Note ->
  Maybe (HI.Splice Identity)
embedResourceRoute model nr embedStack ctx embedderRoute note = do
  let targetRoute = note ^. MN.noteRoute
  if embedStackContains targetRoute embedStack
    then pure $ renderCyclicEmbedSplice model ctx embedStack note
    else do
      let nestedStack = pushEmbedStack targetRoute embedStack
          nestedCtx = withEmbedStack nr model embedderRoute nestedStack ctx
      pure . runEmbedTemplate "note" $ do
        "ema:note:title" ## Tit.titleSplice nestedCtx id (MN._noteTitle note)
        "ema:note:url" ## HI.textSplice (SR.siteRouteUrl model $ SR.lmlSiteRoute (R.LMLView_Html, note ^. MN.noteRoute))
        "ema:note:pandoc" ##
          pandocSplice nestedCtx (note ^. MN.noteDoc)
        "ema:note:toc" ##
          renderToc nestedCtx (newToc $ note ^. MN.noteDoc)

{- | Rebuild a 'HP.RenderCtx' with an augmented embed-ancestor stack baked
into its splice closures, so any nested splice fired from inside a sub-note
sees up-to-date ancestry.

The record update ties the knot: @newCtx@ appears on both the left and inside
its own field assignments. This is well-defined as long as @blockSplice@ /
@inlineSplice@ stay non-strict fields in 'HP.RenderCtx' — at construction
time the fields are stored as thunks closing over @newCtx@, and forcing them
later finds the fully evaluated record. If the upstream @heist-extra@ ever
makes those fields strict the construction would loop; the alternative would
be to wrap the dispatchers in explicit lambdas.
-}
withEmbedStack ::
  PandocRenderers Model R.LMLRoute ->
  Model ->
  R.LMLRoute ->
  EmbedStack R.LMLRoute ->
  HP.RenderCtx ->
  HP.RenderCtx
withEmbedStack nr model x newStack origCtx =
  let newCtx =
        origCtx
          { HP.blockSplice = dispatchBlock model nr newStack newCtx x
          , HP.inlineSplice = dispatchInline model nr newStack newCtx x
          }
   in newCtx

{- | Inline placeholder shown in place of an embed that would form a cycle.

Surfaces the offending note's title plus the chain of ancestor embeds in
deepest-first order, so a reader can see exactly which path closed the loop.
-}
renderCyclicEmbedSplice :: Model -> HP.RenderCtx -> EmbedStack R.LMLRoute -> MN.Note -> HI.Splice Identity
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
