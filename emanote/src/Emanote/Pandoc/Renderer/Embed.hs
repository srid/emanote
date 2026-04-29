module Emanote.Pandoc.Renderer.Embed where

import Commonmark.Extensions.WikiLink qualified as WL
import Data.Map.Syntax ((##))
import Data.Set qualified as Set
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
@Set.insert (note's route) embedStack@, so any nested embed inside this note
sees up-to-date ancestry.
-}
embedResourceRoute ::
  Model ->
  PandocRenderers Model R.LMLRoute ->
  Set R.LMLRoute ->
  HP.RenderCtx ->
  -- | The page being rendered (used to rebuild the ctx for nested splices).
  R.LMLRoute ->
  MN.Note ->
  Maybe (HI.Splice Identity)
embedResourceRoute model nr embedStack ctx pageRoute note = do
  let targetRoute = note ^. MN.noteRoute
  if targetRoute `Set.member` embedStack
    then pure $ renderCyclicEmbedSplice ctx note
    else do
      let nestedStack = Set.insert targetRoute embedStack
          nestedCtx = withEmbedStack nr model pageRoute nestedStack ctx
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
-}
withEmbedStack ::
  PandocRenderers Model R.LMLRoute ->
  Model ->
  R.LMLRoute ->
  Set R.LMLRoute ->
  HP.RenderCtx ->
  HP.RenderCtx
withEmbedStack nr model x newStack origCtx =
  let newCtx =
        origCtx
          { HP.blockSplice = dispatchBlock model nr newStack newCtx x
          , HP.inlineSplice = dispatchInline model nr newStack newCtx x
          }
   in newCtx

-- | Inline placeholder shown in place of an embed that would form a cycle.
renderCyclicEmbedSplice :: HP.RenderCtx -> MN.Note -> HI.Splice Identity
renderCyclicEmbedSplice ctx note =
  rpBlock ctx
    $ B.Div ("", ["emanote:error:cyclic-embed"], [])
    $ one
    $ B.Para
      [ B.Strong [B.Str "↺", B.Space, B.Str "Cyclic embed:", B.Space]
      , B.Code B.nullAttr (Tit.toPlain (note ^. MN.noteTitle))
      ]

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
