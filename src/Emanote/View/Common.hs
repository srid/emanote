{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.View.Common (commonSplices) where

import qualified Data.Aeson.Types as Aeson
import Data.Map.Syntax ((##))
import Data.Version (showVersion)
import qualified Ema.CLI
import qualified Ema.Helper.Tailwind as Tailwind
import qualified Emanote.Model.Note as MN
import qualified Emanote.Model.Title as Tit
import qualified Emanote.View.LiveServerFiles as LiveServerFiles
import qualified Heist as H
import qualified Heist.Interpreted as HI
import qualified Heist.Splices.Apply as HA
import qualified Heist.Splices.Bind as HB
import qualified Heist.Splices.Json as HJ
import qualified Paths_emanote
import Text.Blaze.Html ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Renderer.XmlHtml as RX

commonSplices :: Monad n => Ema.CLI.Action -> Aeson.Value -> Tit.Title -> H.Splices (HI.Splice n)
commonSplices emaAction meta routeTitle = do
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
  "ema:title" ## Tit.titleSplice routeTitle
  -- <head>'s <title> cannot contain HTML
  "ema:titleFull"
    ## Tit.titleSpliceNoHtml routeTitleFull
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
