{-# LANGUAGE RecordWildCards #-}

module Emanote.View.Common
  ( commonSplices,
    mkRendererFromMeta,
    noteRenderers,
    inlineRenderers,
    linkInlineRenderers,
    renderModelTemplate,
    routeBreadcrumbs,
    TemplateRenderCtx (..),
    mkTemplateRenderCtx,
    generatedCssFile,
  )
where

import Data.Aeson.Types qualified as Aeson
import Data.Map.Syntax ((##))
import Data.Text qualified as T
import Data.Version (showVersion)
import Ema qualified
import Emanote.Model.Note qualified as MN
import Emanote.Model.Title qualified as Tit
import Emanote.Model.Type (Model)
import Emanote.Model.Type qualified as M
import Emanote.Pandoc.BuiltinFilters (preparePandoc)
import Emanote.Pandoc.Renderer (PandocBlockRenderer, PandocInlineRenderer, PandocRenderers (..))
import Emanote.Pandoc.Renderer qualified as Renderer
import Emanote.Pandoc.Renderer.Embed qualified as PF
import Emanote.Pandoc.Renderer.Query qualified as PF
import Emanote.Pandoc.Renderer.Url qualified as PF
import Emanote.Route (LMLRoute)
import Emanote.Route qualified as R
import Emanote.Route.SiteRoute.Class qualified as SR
import Emanote.View.LiveServerFiles qualified as LiveServerFiles
import Heist qualified as H
import Heist.Extra.Splices.List qualified as Splices
import Heist.Extra.Splices.Pandoc.Ctx (RenderCtx)
import Heist.Extra.TemplateState qualified as Tmpl
import Heist.Interpreted qualified as HI
import Heist.Splices.Apply qualified as HA
import Heist.Splices.Bind qualified as HB
import Heist.Splices.Json qualified as HJ
import Optics.Operators ((^.))
import Paths_emanote qualified
import Relude
import Text.Blaze.Html ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze.Renderer.XmlHtml qualified as RX

noteRenderers :: Monad n => PandocRenderers n i LMLRoute
noteRenderers =
  PandocRenderers
    _pandocInlineRenderers
    _pandocBlockRenderers

-- | Like `noteRenderers` but for use in inline contexts.
--
-- Backlinks and titles constitute an example of inline context, where we don't
-- care about block elements.
inlineRenderers :: Monad n => PandocRenderers n i b
inlineRenderers =
  PandocRenderers
    _pandocInlineRenderers
    mempty

-- | Like `inlineRenderers` but suitable for use inside links (<a> tags).
linkInlineRenderers :: Monad n => PandocRenderers n i b
linkInlineRenderers =
  PandocRenderers
    _pandocLinkInlineRenderers
    mempty

_pandocInlineRenderers :: Monad n => [PandocInlineRenderer n i b]
_pandocInlineRenderers =
  [ PF.embedInlineWikiLinkResolvingSplice, -- embedInlineWikiLinkResolvingSplice should be first to recognize inline Link elements first
    PF.urlResolvingSplice
  ]
    <> _pandocInlineRenderersCommon

_pandocLinkInlineRenderers :: Monad n => [PandocInlineRenderer n i b]
_pandocLinkInlineRenderers =
  [ PF.plainifyWikiLinkSplice
  ]
    <> _pandocInlineRenderersCommon

_pandocInlineRenderersCommon :: Monad n => [PandocInlineRenderer n i b]
_pandocInlineRenderersCommon =
  []

_pandocBlockRenderers :: Monad n => [PandocBlockRenderer n i LMLRoute]
_pandocBlockRenderers =
  [ PF.embedBlockWikiLinkResolvingSplice,
    PF.queryResolvingSplice
  ]

data TemplateRenderCtx n = TemplateRenderCtx
  { withInlineCtx :: (RenderCtx n -> HI.Splice n) -> HI.Splice n,
    withBlockCtx :: (RenderCtx n -> HI.Splice n) -> HI.Splice n,
    withLinkInlineCtx :: (RenderCtx n -> HI.Splice n) -> HI.Splice n,
    titleSplice :: Tit.Title -> HI.Splice n
  }

mkTemplateRenderCtx ::
  Monad n =>
  Model ->
  R.LMLRoute ->
  Aeson.Value ->
  TemplateRenderCtx n
mkTemplateRenderCtx model r meta =
  let withInlineCtx =
        mkRendererFromMeta model meta inlineRenderers () ()
      withLinkInlineCtx =
        mkRendererFromMeta model meta linkInlineRenderers () ()
      withBlockCtx =
        mkRendererFromMeta model meta noteRenderers () r
      -- TODO: We should be using withInlineCtx, so as to make the wikilink render in note title.
      titleSplice titleDoc = withLinkInlineCtx $ \x ->
        Tit.titleSplice x preparePandoc titleDoc
   in TemplateRenderCtx withInlineCtx withBlockCtx withLinkInlineCtx titleSplice

generatedCssFile :: FilePath
generatedCssFile = "tailwind.css"

commonSplices ::
  HasCallStack =>
  Monad n =>
  ((RenderCtx n -> HI.Splice n) -> HI.Splice n) ->
  Model ->
  Aeson.Value ->
  Tit.Title ->
  H.Splices (HI.Splice n)
commonSplices withCtx model meta routeTitle = do
  let siteTitle = fromString . toString $ MN.lookupAeson @Text "Emabook Site" ("page" :| ["siteTitle"]) meta
      routeTitleFull =
        if routeTitle == siteTitle
          then siteTitle
          else routeTitle <> " – " <> siteTitle
  -- Heist helpers
  "bind" ## HB.bindImpl
  "apply" ## HA.applyImpl
  -- Add tailwind css shim
  "tailwindCssShim"
    ## do
      pure . RX.renderHtmlNodes $
        if M.inLiveServer model
          then do
            -- Twind shim doesn't reliably work in dev server mode. Let's just use the
            -- tailwind CDN.
            cachedTailwindCdn
          else do
            H.link
              -- TODO: Use ?md5 to prevent stale browser caching of CSS.
              ! A.href (H.toValue $ cannotBeCached generatedCssFile)
              ! A.rel "stylesheet"
              ! A.type_ "text/css"
  "ema:version"
    ## HI.textSplice (toText $ showVersion Paths_emanote.version)
  "ema:metadata"
    ## HJ.bindJson meta
  "ema:title" ## withCtx $ \ctx ->
    Tit.titleSplice ctx preparePandoc routeTitle
  -- <head>'s <title> cannot contain HTML
  "ema:titleFull"
    ## Tit.titleSpliceNoHtml routeTitleFull
  "ema:homeUrl"
    ## ( let homeR = SR.lmlSiteRoute $ R.liftLMLRoute @('R.LMLType 'R.Md) R.indexRoute
          in HI.textSplice (SR.siteRouteUrl model homeR)
       )
  "ema:indexUrl"
    ## HI.textSplice (SR.siteRouteUrl model SR.indexRoute)
  "ema:tagIndexUrl"
    ## HI.textSplice (SR.siteRouteUrl model $ SR.tagIndexRoute [])
  "ema:taskIndexUrl"
    ## HI.textSplice (SR.siteRouteUrl model SR.taskIndexRoute)
  "ema:emanoteStaticLayerUrl"
    ## HI.textSplice
      ( -- HACK
        -- Also: more-head.tpl is the one place where this is hardcoded.
        let itUrl =
              SR.siteRouteUrl model $
                SR.staticFileSiteRoute $
                  fromMaybe (error "no _emanote-static?") $ M.modelLookupStaticFile "_emanote-static/inverted-tree.css" model
         in fst $ T.breakOn "/inverted-tree.css" itUrl
      )
  -- For those cases the user really wants to hardcode the URL
  "ema:urlStrategySuffix"
    ## HI.textSplice (SR.urlStrategySuffix model)
  where
    -- A hack to force the browser not to cache the CSS, because we are not md5
    -- hashing the CSS yet (because the CSS is generated *after* the HTML files
    -- are generated.)
    -- For a proper way to do this, see: https://github.com/srid/ema/issues/20
    cannotBeCached url = url <> "?instanceId=" <> show (model ^. M.modelInstanceID)
    cachedTailwindCdn =
      -- TODO: abstract this out, and use it to fetch favicon logo (iconUrl)
      -- But this comes from index.yaml, and used .tpl files. No Haskell involved. Perhaps we can patch index.yaml meta accordingly ...
      let localCdnUrl = SR.siteRouteUrl model $ SR.staticFileSiteRoute $ fromMaybe (error "model not ready?") $ M.modelLookupStaticFile LiveServerFiles.tailwindFullCssPath model
       in H.link
            ! A.href (H.toValue localCdnUrl)
            ! A.rel "stylesheet"
            ! A.type_ "text/css"

-- | Given a route metadata, return the context generating function that can be
-- used to render an arbitrary Pandoc AST
--
-- The returned function allows specifing a `PandocRenderers` type, along with
-- the associated data arguments.
mkRendererFromMeta ::
  (Monad m, Monad n) =>
  Model ->
  Aeson.Value ->
  ( PandocRenderers n i b ->
    i ->
    b ->
    (RenderCtx n -> H.HeistT n m x) ->
    H.HeistT n m x
  )
mkRendererFromMeta model routeMeta =
  let classRules = MN.lookupAeson @(Map Text Text) mempty ("pandoc" :| ["rewriteClass"]) routeMeta
   in mkRendererWith model classRules

mkRendererWith ::
  (Monad m, Monad n) =>
  Model ->
  Map Text Text ->
  ( PandocRenderers n i b ->
    i ->
    b ->
    (RenderCtx n -> H.HeistT n m x) ->
    H.HeistT n m x
  )
mkRendererWith model classRules =
  let withNoteRenderer nr i b f = do
        renderCtx <-
          Renderer.mkRenderCtxWithPandocRenderers
            nr
            classRules
            model
            i
            b
        f renderCtx
   in withNoteRenderer

renderModelTemplate :: Model -> Tmpl.TemplateName -> H.Splices (HI.Splice Identity) -> LByteString
renderModelTemplate model templateName =
  let handleErr =
        if M.inLiveServer model
          then Ema.emaErrorHtmlResponse
          else -- When staticaly generating, we must fail asap on template errors.
            error
   in -- Until Ema's error handling improves ...
      either handleErr id
        . flip (Tmpl.renderHeistTemplate templateName) (model ^. M.modelHeistTemplate)

routeBreadcrumbs :: Monad n => TemplateRenderCtx n -> Model -> LMLRoute -> HI.Splice n
routeBreadcrumbs TemplateRenderCtx {..} model r =
  Splices.listSplice (init $ R.routeInits . R.lmlRouteCase $ r) "each-crumb" $
    \(R.liftLMLRoute -> crumbR) -> do
      "crumb:url" ## HI.textSplice (SR.siteRouteUrl model $ SR.lmlSiteRoute crumbR)
      "crumb:title" ## titleSplice (M.modelLookupTitle crumbR model)
