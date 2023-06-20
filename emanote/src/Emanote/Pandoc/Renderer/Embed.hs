module Emanote.Pandoc.Renderer.Embed where

import Commonmark.Extensions.WikiLink qualified as WL
import Data.Map.Syntax ((##))
import Emanote.Model (Model)
import Emanote.Model.Link.Rel qualified as Rel
import Emanote.Model.Link.Resolve qualified as Resolve
import Emanote.Model.Note qualified as MN
import Emanote.Model.StaticFile (CodeLanguage (..), StaticFileInfo (..), staticFileInfoToName)
import Emanote.Model.StaticFile qualified as SF
import Emanote.Model.Title qualified as Tit
import Emanote.Pandoc.BuiltinFilters (prepareNoteDoc, preparePandoc)
import Emanote.Pandoc.Link qualified as Link
import Emanote.Pandoc.Renderer (PandocBlockRenderer, PandocInlineRenderer)
import Emanote.Pandoc.Renderer.Url qualified as RenderedUrl
import Emanote.Route.ModelRoute qualified as R
import Emanote.Route.R qualified as R
import Emanote.Route.SiteRoute qualified as SF
import Emanote.Route.SiteRoute qualified as SR
import Heist qualified as H
import Heist.Extra qualified as HE
import Heist.Extra.Splices.Pandoc (pandocSplice)
import Heist.Extra.Splices.Pandoc qualified as HP
import Heist.Interpreted qualified as HI
import Optics.Operators ((^.))
import Relude
import Text.Pandoc.Definition qualified as B

embedBlockWikiLinkResolvingSplice :: PandocBlockRenderer Model R.LMLRoute
embedBlockWikiLinkResolvingSplice model _nf ctx noteRoute node = do
  B.Para [inl] <- pure node
  (inlRef, (_, _, otherAttrs), is, (url, tit)) <- Link.parseInlineRef inl
  guard $ inlRef == Link.InlineLink
  let parentR = R.withLmlRoute R.routeParent noteRoute
  -- TODO: Use anchor to embed a section?
  (Rel.URTWikiLink (WL.WikiLinkEmbed, wl), _mAnchor) <-
    Rel.parseUnresolvedRelTarget parentR (otherAttrs <> one ("title", tit)) url
  let rRel = Resolve.resolveWikiLinkMustExist model wl
  RenderedUrl.renderSomeInlineRefWith Resolve.resourceSiteRoute (is, (url, tit)) rRel model ctx inl $
    either (embedResourceRoute model ctx) (const Nothing)

embedBlockRegularLinkResolvingSplice :: PandocBlockRenderer Model R.LMLRoute
embedBlockRegularLinkResolvingSplice model _nf ctx noteRoute node = do
  B.Para [inl] <- pure node
  (inlRef, (_, _, otherAttrs), is, (url, tit)) <- Link.parseInlineRef inl
  guard $ inlRef == Link.InlineImage
  let parentR = R.withLmlRoute R.routeParent noteRoute
  (Rel.URTResource mr, _mAnchor) <-
    Rel.parseUnresolvedRelTarget parentR (otherAttrs <> one ("title", tit)) url
  let rRel = Resolve.resolveModelRoute model mr
  RenderedUrl.renderSomeInlineRefWith Resolve.resourceSiteRoute (is, (url, tit)) rRel model ctx inl $
    either (const Nothing) (embedStaticFileRoute model $ WL.plainify is)

embedInlineWikiLinkResolvingSplice :: PandocInlineRenderer Model R.LMLRoute
embedInlineWikiLinkResolvingSplice model _nf ctx noteRoute inl = do
  (inlRef, (_, _, otherAttrs), is, (url, tit)) <- Link.parseInlineRef inl
  guard $ inlRef == Link.InlineLink
  let parentR = R.withLmlRoute R.routeParent noteRoute
  (Rel.URTWikiLink (WL.WikiLinkEmbed, wl), _mAnchor) <- Rel.parseUnresolvedRelTarget parentR (otherAttrs <> one ("title", tit)) url
  let rRel = Resolve.resolveWikiLinkMustExist model wl
  RenderedUrl.renderSomeInlineRefWith Resolve.resourceSiteRoute (is, (url, tit)) rRel model ctx inl $
    either (const Nothing) (embedStaticFileRoute model $ show wl)

runEmbedTemplate :: ByteString -> H.Splices (HI.Splice Identity) -> HI.Splice Identity
runEmbedTemplate name splices = do
  tpl <- HE.lookupHtmlTemplateMust $ "/templates/filters/embed-" <> name
  HE.runCustomTemplate tpl splices

embedResourceRoute :: Model -> HP.RenderCtx -> MN.Note -> Maybe (HI.Splice Identity)
embedResourceRoute model ctx note = do
  pure . runEmbedTemplate "note" $ do
    "ema:note:title" ## Tit.titleSplice ctx preparePandoc (MN._noteTitle note)
    "ema:note:url" ## HI.textSplice (SR.siteRouteUrl model $ SR.lmlSiteRoute $ note ^. MN.noteRoute)
    "ema:note:pandoc" ##
      pandocSplice ctx (prepareNoteDoc note)

embedStaticFileRoute :: Model -> Text -> SF.StaticFile -> Maybe (HI.Splice Identity)
embedStaticFileRoute model altText staticFile = do
  let url = SF.siteRouteUrl model $ SF.staticFileSiteRoute staticFile

  staticFileInfo <- SF._staticFileInfo staticFile

  pure . runEmbedTemplate (staticFileInfoToName staticFileInfo) $ do
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
        "ema:content" ## HI.textSplice content
        "ema:language" ## HI.textSplice language
