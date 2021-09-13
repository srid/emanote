{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.View.Common
  ( commonSplices,
    mkRendererFromMeta,
    noteRenderers,
    inlineRenderers,
    linkInlineRenderers,
    renderModelTemplate,
  )
where

import Control.Lens.Operators ((^.))
import qualified Data.Aeson.Types as Aeson
import Data.Map.Syntax ((##))
import Data.Version (showVersion)
import qualified Ema
import qualified Ema.CLI
import qualified Ema.Helper.Tailwind as Tailwind
import qualified Emanote.Model.Note as MN
import qualified Emanote.Model.Title as Tit
import Emanote.Model.Type (Model)
import qualified Emanote.Model.Type as M
import Emanote.Pandoc.BuiltinFilters (preparePandoc)
import Emanote.Pandoc.Renderer (PandocBlockRenderer, PandocInlineRenderer, PandocRenderers (..))
import qualified Emanote.Pandoc.Renderer as Renderer
import qualified Emanote.Pandoc.Renderer.Embed as PF
import qualified Emanote.Pandoc.Renderer.Query as PF
import qualified Emanote.Pandoc.Renderer.Url as PF
import Emanote.Route (LMLRoute)
import qualified Emanote.Route.SiteRoute.Class as SR
import qualified Emanote.View.LiveServerFiles as LiveServerFiles
import qualified Heist as H
import Heist.Extra.Splices.Pandoc.Ctx (RenderCtx)
import qualified Heist.Extra.TemplateState as Tmpl
import qualified Heist.Interpreted as HI
import qualified Heist.Splices.Apply as HA
import qualified Heist.Splices.Bind as HB
import qualified Heist.Splices.Json as HJ
import qualified Paths_emanote
import Text.Blaze.Html ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Renderer.XmlHtml as RX

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

commonSplices ::
  Monad n =>
  ((RenderCtx n -> HI.Splice n) -> HI.Splice n) ->
  Ema.CLI.Action ->
  Model ->
  Aeson.Value ->
  Tit.Title ->
  H.Splices (HI.Splice n)
commonSplices withCtx emaAction model meta routeTitle = do
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
    ## pure
      (RX.renderHtmlNodes $ twindShim emaAction)
  "ema:version"
    ## HI.textSplice (toText $ showVersion Paths_emanote.version)
  "ema:metadata"
    ## HJ.bindJson meta
  "ema:title" ## withCtx $ \ctx ->
    Tit.titleSplice ctx (preparePandoc model) routeTitle
  -- <head>'s <title> cannot contain HTML
  "ema:titleFull"
    ## Tit.titleSpliceNoHtml routeTitleFull
  "ema:indexUrl"
    ## HI.textSplice (SR.siteRouteUrl model SR.indexRoute)
  "ema:tagIndexUrl"
    ## HI.textSplice (SR.siteRouteUrl model $ SR.tagIndexRoute [])
  -- For those cases the user really wants to hardcode the URL
  "ema:urlStrategySuffix"
    ## HI.textSplice (SR.urlStrategySuffix model)
  where
    twindShim :: Ema.CLI.Action -> H.Html
    twindShim action =
      case action of
        Ema.CLI.Generate _ ->
          Tailwind.twindShimUnofficial
        _ ->
          -- Twind shim doesn't reliably work in dev server mode. Let's just use the
          -- tailwind CDN.
          cachedTailwindCdn
    cachedTailwindCdn =
      H.link
        ! A.href (H.toValue LiveServerFiles.tailwindFullCssUrl)
        ! A.rel "stylesheet"
        ! A.type_ "text/css"

-- | Given a route metadata, return the context generating function that can be
-- used to render an arbitrary Pandoc AST
--
-- The returned function allows specifing a `PandocRenderers` type, along with
-- the associated data arguments.
mkRendererFromMeta ::
  (Monad m, Monad n) =>
  Ema.CLI.Action ->
  Model ->
  Aeson.Value ->
  ( PandocRenderers n i b ->
    i ->
    b ->
    (RenderCtx n -> H.HeistT n m x) ->
    H.HeistT n m x
  )
mkRendererFromMeta emaAction model routeMeta =
  let classRules = MN.lookupAeson @(Map Text Text) mempty ("pandoc" :| ["rewriteClass"]) routeMeta
   in mkRendererWith emaAction model classRules

mkRendererWith ::
  (Monad m, Monad n) =>
  Ema.CLI.Action ->
  Model ->
  Map Text Text ->
  ( PandocRenderers n i b ->
    i ->
    b ->
    (RenderCtx n -> H.HeistT n m x) ->
    H.HeistT n m x
  )
mkRendererWith emaAction model classRules =
  let withNoteRenderer nr i b f = do
        renderCtx <-
          Renderer.mkRenderCtxWithPandocRenderers
            nr
            classRules
            emaAction
            model
            i
            b
        f renderCtx
   in withNoteRenderer

renderModelTemplate :: Ema.CLI.Action -> Model -> Tmpl.TemplateName -> H.Splices (HI.Splice Identity) -> LByteString
renderModelTemplate emaAction model templateName =
  let handleErr = case emaAction of
        Ema.CLI.Run -> Ema.emaErrorHtmlResponse
        -- When staticaly generating, we must fail asap on template errors.
        _ -> error
   in -- Until Ema's error handling improves ...
      either handleErr id
        . flip (Tmpl.renderHeistTemplate templateName) (model ^. M.modelHeistTemplate)