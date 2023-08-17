{-# LANGUAGE RecordWildCards #-}

module Emanote.View.Common (
  commonSplices,
  renderModelTemplate,
  routeBreadcrumbs,
  generatedCssFile,

  -- * Render context
  TemplateRenderCtx (..),
  mkTemplateRenderCtx,
  defaultRouteMeta,
) where

import Data.Aeson.Types qualified as Aeson
import Data.Map.Syntax ((##))
import Data.Text qualified as T
import Data.Version (showVersion)
import Ema qualified
import Emanote.Model.Meta qualified as Meta
import Emanote.Model.SData qualified as SData
import Emanote.Model.Title qualified as Tit
import Emanote.Model.Type (Model)
import Emanote.Model.Type qualified as M
import Emanote.Pandoc.Renderer (EmanotePandocRenderers (..), PandocRenderers (..))
import Emanote.Pandoc.Renderer qualified as Renderer
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
import System.FilePath ((</>))
import Text.Blaze.Html ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze.Renderer.XmlHtml qualified as RX

data TemplateRenderCtx n = TemplateRenderCtx
  { withInlineCtx :: (RenderCtx -> HI.Splice Identity) -> HI.Splice Identity
  , withBlockCtx :: (RenderCtx -> HI.Splice Identity) -> HI.Splice Identity
  , withLinkInlineCtx :: (RenderCtx -> HI.Splice Identity) -> HI.Splice Identity
  , titleSplice :: Tit.Title -> HI.Splice Identity
  }

{- | Create the context in which Heist templates (notably `pandoc.tpl`) will be
 rendered.
-}
mkTemplateRenderCtx ::
  -- | Current model.
  Model ->
  -- | Current route.
  R.LMLRoute ->
  -- | Associated metadata.
  Aeson.Value ->
  TemplateRenderCtx Identity
mkTemplateRenderCtx model r meta =
  let EmanotePandocRenderers {..} = M._modelPandocRenderers model
      withInlineCtx =
        withRenderCtx inlineRenderers
      withLinkInlineCtx =
        withRenderCtx linkInlineRenderers
      withBlockCtx =
        withRenderCtx blockRenderers
      -- TODO: We should be using withInlineCtx, so as to make the wikilink
      -- render in note title.
      titleSplice titleDoc = withLinkInlineCtx $ \ctx ->
        Tit.titleSplice ctx id titleDoc
   in TemplateRenderCtx {..}
  where
    withRenderCtx ::
      (Monad m) =>
      PandocRenderers Model LMLRoute ->
      (RenderCtx -> H.HeistT Identity m x) ->
      H.HeistT Identity m x
    withRenderCtx pandocRenderers f =
      f
        =<< Renderer.mkRenderCtxWithPandocRenderers
          pandocRenderers
          classRules
          model
          r
    classRules :: Map Text Text
    classRules =
      SData.lookupAeson mempty ("pandoc" :| ["rewriteClass"]) meta

defaultRouteMeta :: Model -> (LMLRoute, Aeson.Value)
defaultRouteMeta model =
  let r = M.modelIndexRoute model
      meta = Meta.getEffectiveRouteMeta r model
   in (r, meta)

generatedCssFile :: FilePath
generatedCssFile = "tailwind.css"

commonSplices ::
  (HasCallStack) =>
  ((RenderCtx -> HI.Splice Identity) -> HI.Splice Identity) ->
  Model ->
  Aeson.Value ->
  Tit.Title ->
  H.Splices (HI.Splice Identity)
commonSplices withCtx model meta routeTitle = do
  let siteTitle = fromString . toString $ SData.lookupAeson @Text "Emanote Site" ("page" :| ["siteTitle"]) meta
      routeTitleFull =
        if routeTitle == siteTitle
          then siteTitle
          else routeTitle <> " – " <> siteTitle
  -- Heist helpers
  "bind" ## HB.bindImpl
  "apply" ## HA.applyImpl
  -- Add tailwind css shim
  "tailwindCssShim" ##
    do
      pure . RX.renderHtmlNodes $
        if M.inLiveServer model || not (model ^. M.modelCompileTailwind)
          then do
            -- Twind shim doesn't reliably work in dev server mode. Let's just use the
            -- tailwind CDN.
            cachedTailwindCdn
          else do
            H.link
              -- TODO: Use ?md5 to prevent stale browser caching of CSS.
              -- TODO: This should go through Ema route encoder!
              ! A.href (H.toValue $ cannotBeCached generatedCssFile)
              ! A.rel "stylesheet"
              ! A.type_ "text/css"
  "ema:version" ##
    HI.textSplice (toText $ showVersion Paths_emanote.version)
  "ema:metadata" ##
    HJ.bindJson meta
  "ema:title" ## withCtx $ \ctx ->
    Tit.titleSplice ctx id routeTitle
  -- <head>'s <title> cannot contain HTML
  "ema:titleFull" ##
    Tit.titleSpliceNoHtml routeTitleFull
  -- `ema:homeUrl` is normally `""`; but if Emanote is being served from an URL
  -- prefix, it would be "/foo/" (with a slash at the end). This allows you to
  -- just concatanate homeUrl with a relative URL path (no slash in between), to
  -- get the full URL. The reason there is no slash in between is to account for
  -- the usual case of homeUrl being an empty string.
  "ema:homeUrl" ##
    ( let homeR = SR.lmlSiteRoute (M.modelIndexRoute model)
          homeUrl' = SR.siteRouteUrl model homeR
          homeUrl = if homeUrl' /= "" then homeUrl' <> "/" else homeUrl'
       in HI.textSplice homeUrl
    )
  "ema:indexUrl" ##
    HI.textSplice (SR.siteRouteUrl model SR.indexRoute)
  "ema:tagIndexUrl" ##
    HI.textSplice (SR.siteRouteUrl model $ SR.tagIndexRoute [])
  "ema:taskIndexUrl" ##
    HI.textSplice (SR.siteRouteUrl model SR.taskIndexRoute)
  "ema:emanoteStaticLayerUrl" ##
    HI.textSplice
      ( -- HACK
        -- Also: more-head.tpl is the one place where this is hardcoded.
        let staticFolder = "_emanote-static"
            itUrl =
              SR.siteRouteUrl model $
                SR.staticFileSiteRoute $
                  fromMaybe (error "no _emanote-static?") $
                    M.modelLookupStaticFile (staticFolder </> "inverted-tree.css") model
            staticFolderUrl = fst $ T.breakOn "/inverted-tree.css" itUrl
            -- Deal with a silly Firefox bug https://github.com/srid/emanote/issues/340
            --
            -- Firefox deduces an incorrect <base> after doing morphdom
            -- patching, unless the <base> is absolute (i.e., starts with a '/').
            patchForFirefoxBug folder url =
              if M.inLiveServer model && url == toText folder
                then "/" <> url
                else url
         in patchForFirefoxBug staticFolder staticFolderUrl
      )
  -- For those cases the user really wants to hardcode the URL
  "ema:urlStrategySuffix" ##
    HI.textSplice (SR.urlStrategySuffix model)
  where
    -- A hack to force the browser not to cache the CSS, because we are not md5
    -- hashing the CSS yet (because the CSS is generated *after* the HTML files
    -- are generated.)
    -- For a proper way to do this, see: https://github.com/srid/ema/issues/20
    cannotBeCached url = url <> "?instanceId=" <> show (model ^. M.modelInstanceID)
    cachedTailwindCdn = do
      let localCdnUrl =
            SR.siteRouteUrl model $
              SR.staticFileSiteRoute $
                LiveServerFiles.tailwindCssFile model
      H.link
        ! A.href (H.toValue localCdnUrl)
        ! A.rel "stylesheet"
        ! A.type_ "text/css"

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

routeBreadcrumbs :: TemplateRenderCtx n -> Model -> LMLRoute -> HI.Splice Identity
routeBreadcrumbs TemplateRenderCtx {..} model r = do
  let breadcrumbs =
        r
          & R.lmlRouteCase
          -- Hardcode to 'Md, and resolve using resolveLmlRoute latter.
          & either R.routeInits (R.routeInits . coerce)
          & init
          & fmap (M.resolveLmlRoute model)
  Splices.listSplice breadcrumbs "each-crumb" $ \crumbR -> do
    "crumb:url" ## HI.textSplice (SR.siteRouteUrl model $ SR.lmlSiteRoute crumbR)
    "crumb:title" ## titleSplice (M.modelLookupTitle crumbR model)
