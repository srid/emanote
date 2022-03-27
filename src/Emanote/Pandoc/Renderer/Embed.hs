module Emanote.Pandoc.Renderer.Embed where

import Data.Map.Syntax ((##))
import Data.Text qualified as T
import Emanote.Model (Model)
import Emanote.Model.Link.Rel qualified as Rel
import Emanote.Model.Link.Resolve qualified as Resolve
import Emanote.Model.Note qualified as MN
import Emanote.Model.StaticFile qualified as SF
import Emanote.Model.Title qualified as Tit
import Emanote.Pandoc.BuiltinFilters (prepareNoteDoc, preparePandoc)
import Emanote.Pandoc.Link qualified as Link
import Emanote.Pandoc.Markdown.Syntax.WikiLink qualified as WL
import Emanote.Pandoc.Renderer (PandocBlockRenderer, PandocInlineRenderer)
import Emanote.Pandoc.Renderer.Url qualified as RenderedUrl
import Emanote.Route.ModelRoute (LMLRoute)
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

embedBlockWikiLinkResolvingSplice ::
  Monad n => PandocBlockRenderer n i LMLRoute
embedBlockWikiLinkResolvingSplice model _nf ctx noteRoute = \case
  B.Para [inl] -> do
    (inlRef, (_, _, otherAttrs), is, (url, tit)) <- Link.parseInlineRef inl
    guard $ inlRef == Link.InlineLink
    let parentR = R.routeParent $ R.lmlRouteCase noteRoute
    -- TODO: Use anchor to embed a section?
    (Rel.URTWikiLink (WL.WikiLinkEmbed, wl), _mAnchor) <-
      Rel.parseUnresolvedRelTarget parentR (otherAttrs <> one ("title", tit)) url
    let rRel = Resolve.resolveWikiLinkMustExist model wl
    let f = embedBlockSiteRoute model ctx
    RenderedUrl.renderSomeInlineRefWith f Resolve.resourceSiteRoute (is, (url, tit)) rRel model ctx inl
  _ ->
    Nothing

embedInlineWikiLinkResolvingSplice ::
  Monad n => PandocInlineRenderer n LMLRoute b
embedInlineWikiLinkResolvingSplice model _nf ctx noteRoute inl = do
  (inlRef, (_, _, otherAttrs), is, (url, tit)) <- Link.parseInlineRef inl
  guard $ inlRef == Link.InlineLink
  let parentR = R.routeParent $ R.lmlRouteCase noteRoute
  (Rel.URTWikiLink (WL.WikiLinkEmbed, wl), _mAnchor) <- Rel.parseUnresolvedRelTarget parentR (otherAttrs <> one ("title", tit)) url
  let rRel = Resolve.resolveWikiLinkMustExist model wl
      f = embedInlineSiteRoute model wl
  RenderedUrl.renderSomeInlineRefWith f Resolve.resourceSiteRoute (is, (url, tit)) rRel model ctx inl

embedBlockSiteRoute :: Monad n => Model -> HP.RenderCtx n -> Either MN.Note SF.StaticFile -> Maybe (HI.Splice n)
embedBlockSiteRoute model ctx = either (embedResourceRoute model ctx) (const Nothing)

embedInlineSiteRoute :: Monad n => Model -> WL.WikiLink -> Either MN.Note SF.StaticFile -> Maybe (HI.Splice n)
embedInlineSiteRoute model wl = either (const Nothing) (embedStaticFileRoute model wl)

runEmbedTemplate :: Monad n => ByteString -> H.Splices (HI.Splice n) -> HI.Splice n
runEmbedTemplate name splices = do
  tpl <- HE.lookupHtmlTemplateMust $ "/templates/filters/embed-" <> name
  HE.runCustomTemplate tpl splices

embedResourceRoute :: Monad n => Model -> HP.RenderCtx n -> MN.Note -> Maybe (HI.Splice n)
embedResourceRoute model ctx note = do
  pure . runEmbedTemplate "note" $ do
    "ema:note:title" ## Tit.titleSplice ctx preparePandoc (MN._noteTitle note)
    "ema:note:url" ## HI.textSplice (SR.siteRouteUrl model $ SR.lmlSiteRoute $ note ^. MN.noteRoute)
    "ema:note:pandoc"
      ## pandocSplice ctx (prepareNoteDoc $ MN._noteDoc note)

embedStaticFileRoute :: Monad n => Model -> WL.WikiLink -> SF.StaticFile -> Maybe (HI.Splice n)
embedStaticFileRoute model wl staticFile = do
  let fp = staticFile ^. SF.staticFilePath
      url = SF.siteRouteUrl model $ SF.staticFileSiteRoute staticFile
  if
      | any (`T.isSuffixOf` toText fp) imageExts ->
        pure . runEmbedTemplate "image" $ do
          "ema:url" ## HI.textSplice url
          "ema:alt" ## HI.textSplice $ show wl
      | any (`T.isSuffixOf` toText fp) videoExts -> do
        pure . runEmbedTemplate "video" $ do
          "ema:url" ## HI.textSplice url
      | otherwise -> Nothing

imageExts :: [Text]
imageExts =
  [ ".jpg"
  , ".jpeg"
  , ".png"
  , ".svg"
  , ".gif"
  , ".bmp"
  , ".webp"
  ]

videoExts :: [Text]
videoExts =
  [ ".mp4"
  , ".webm"
  , ".ogv"
  ]
