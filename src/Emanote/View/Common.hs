{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.View.Common
  ( commonSplices,
    noteRenderers,
    getRouteContexts,
    inlineNoteRenderers,
  )
where

import qualified Data.Aeson.Types as Aeson
import Data.Map.Syntax ((##))
import Data.Version (showVersion)
import qualified Ema.CLI
import qualified Ema.Helper.Tailwind as Tailwind
import qualified Emanote.Model.Note as MN
import qualified Emanote.Model.Title as Tit
import Emanote.Model.Type (Model)
import Emanote.Pandoc.BuiltinFilters (preparePandoc)
import Emanote.Pandoc.Renderer (NoteRenderers (..), PandocInlineRenderer)
import qualified Emanote.Pandoc.Renderer as Renderer
import qualified Emanote.Pandoc.Renderer.Embed as PF
import qualified Emanote.Pandoc.Renderer.Query as PF
import qualified Emanote.Pandoc.Renderer.Url as PF
import Emanote.Route (LMLRoute)
import qualified Emanote.Route.SiteRoute.Class as SR
import qualified Emanote.View.LiveServerFiles as LiveServerFiles
import qualified Heist as H
import Heist.Extra.Splices.Pandoc.Ctx (RenderCtx)
import qualified Heist.Interpreted as HI
import qualified Heist.Splices.Apply as HA
import qualified Heist.Splices.Bind as HB
import qualified Heist.Splices.Json as HJ
import qualified Paths_emanote
import Text.Blaze.Html ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Renderer.XmlHtml as RX

-- | Custom renderers for Pandoc notes
--
-- Configure the default Pandoc splices here.
noteRenderers :: Monad n => NoteRenderers n i LMLRoute
noteRenderers =
  NoteRenderers
    pandocInlineRenderers
    [ PF.embedWikiLinkResolvingSplice,
      PF.queryResolvingSplice
    ]

-- | Like `noteRenderers` but for use in inline contexts.
inlineNoteRenderers :: Monad n => NoteRenderers n i b
inlineNoteRenderers =
  NoteRenderers
    pandocInlineRenderers
    -- Remove block filters, in render contexts where we expect to show inline
    -- content (eg: backlinks and titles)
    mempty

pandocInlineRenderers :: Monad n => [PandocInlineRenderer n i b x]
pandocInlineRenderers =
  [ PF.urlResolvingSplice
  ]

getRouteContexts ::
  (Monad m, Monad n) =>
  Ema.CLI.Action ->
  Model ->
  Aeson.Value ->
  ( Renderer.NoteRenderers n i b ->
    i ->
    b ->
    (RenderCtx n -> H.HeistT n m x) ->
    H.HeistT n m x
  )
getRouteContexts emaAction model meta =
  let classRules = MN.lookupAeson @(Map Text Text) mempty ("pandoc" :| ["rewriteClass"]) meta
      withNoteRenderer nr i b f = do
        renderCtx <-
          Renderer.mkRenderCtxWithNoteRenderers
            nr
            classRules
            emaAction
            model
            i
            b
        f renderCtx
   in withNoteRenderer

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
