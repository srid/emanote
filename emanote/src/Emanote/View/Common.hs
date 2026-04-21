{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Emanote.View.Common (
  commonSplices,
  renderModelTemplate,
  routeBreadcrumbs,
  generatedCssFile,
  tailwindInputCss,

  -- * Render context
  TemplateRenderCtx (..),
  mkTemplateRenderCtx,
  defaultRouteMeta,
)
where

import Data.Aeson.Types qualified as Aeson
import Data.Map.Syntax ((##))
import Data.Text qualified as T
import Data.Version (showVersion)
import Ema.Server.Common qualified as Ema
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
import NeatInterpolation (text)
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
          enableSyntaxHighlighting
    classRules :: Map Text Text
    classRules =
      SData.lookupAeson mempty ("pandoc" :| ["rewriteClass"]) meta
    enableSyntaxHighlighting :: Bool
    enableSyntaxHighlighting =
      SData.lookupAeson True ("emanote" :| ["syntaxHighlighting"]) meta

defaultRouteMeta :: Model -> (LMLRoute, Aeson.Value)
defaultRouteMeta model =
  let r = M.modelIndexRoute model
      meta = Meta.getEffectiveRouteMeta r model
   in (r, meta)

generatedCssFile :: FilePath
generatedCssFile = "tailwind.css"

{- | Base Tailwind v4 input CSS.

Registers a 'primary' color token (all eleven scale levels) as CSS variables
aliased to the Tailwind blue palette at compile time. At runtime, a
per-site @<style>@ block (emitted by 'tailwindCssShim') overrides the aliases
to the site's configured @template.theme@ palette, so a site's theme color
changes without regenerating the compiled CSS.
-}
tailwindInputCss :: Text
tailwindInputCss =
  [text|
    @import "tailwindcss";
    @plugin "@tailwindcss/typography";

    @theme {
      --color-primary-50:  var(--color-blue-50);
      --color-primary-100: var(--color-blue-100);
      --color-primary-200: var(--color-blue-200);
      --color-primary-300: var(--color-blue-300);
      --color-primary-400: var(--color-blue-400);
      --color-primary-500: var(--color-blue-500);
      --color-primary-600: var(--color-blue-600);
      --color-primary-700: var(--color-blue-700);
      --color-primary-800: var(--color-blue-800);
      --color-primary-900: var(--color-blue-900);
      --color-primary-950: var(--color-blue-950);
    }
  |]

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
      let themeName = SData.lookupAeson @Text "blue" ("template" :| ["theme"]) meta
      pure
        . RX.renderHtmlNodes
        $ do
          -- Per-site theme remap: aliases the 'primary' color token to the
          -- configured 'template.theme' palette at runtime. Keeps the compiled
          -- Tailwind CSS agnostic of which palette a site picked.
          themeRemapStyle themeName
          if M.inLiveServer model || not (model ^. M.modelCompileTailwind)
            then do
              -- Dev / uncompiled path: use the Tailwind v4 browser CDN shim.
              cachedTailwindCdn
              H.style ! A.type_ "text/tailwindcss" $ H.toHtml tailwindInputCss
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
  "ema:title" ##
    withCtx
      $ \ctx ->
        Tit.titleSplice ctx id routeTitle
  -- <head>'s <title> cannot contain HTML
  "ema:titleFull" ##
    Tit.titleSpliceNoHtml routeTitleFull
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
              SR.siteRouteUrl model
                $ SR.staticFileSiteRoute
                $ fromMaybe (error "no _emanote-static?")
                $ M.modelLookupStaticFile (staticFolder </> "inverted-tree.css") model
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
            SR.siteRouteUrl model
              $ SR.staticFileSiteRoute
              $ LiveServerFiles.tailwindJsFile model
      H.script
        ! H.customAttribute "data-ema-skip" "true"
        ! A.src (H.toValue localCdnUrl)
        $ mempty
    themeRemapStyle themeName =
      let remap =
            unlines
              [ "  --color-primary-" <> lvl <> ": var(--color-" <> themeName <> "-" <> lvl <> ");"
              | lvl <- ["50", "100", "200", "300", "400", "500", "600", "700", "800", "900", "950"]
              ]
          css = ":root {\n" <> remap <> "}\n"
       in H.style $ H.toHtml css

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
    "crumb:url" ## HI.textSplice (SR.siteRouteUrl model $ SR.lmlSiteRoute (R.LMLView_Html, crumbR))
    "crumb:title" ## titleSplice (M.modelLookupTitle crumbR model)
