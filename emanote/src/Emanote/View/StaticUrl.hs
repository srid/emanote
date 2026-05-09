{- | Cache-busted URL construction for assets under the bundled
@_emanote-static@ layer. Routes every lookup through 'SR.siteRouteUrl'
so live-server mode appends @?t=\<mtime\>@ and edits invalidate the
browser cache without a manual restart.

The Heist surface is 'emanoteStaticUrlSplice' — an attributed
element-form splice that reads a @path@ attribute and runs its body
with @${url}@ bound to the cache-busted URL. Templates wrap the
consuming tag in it:

> <emanoteStaticUrl path="skylighting.css">
>   <link rel="stylesheet" href="${url}" />
> </emanoteStaticUrl>

The plain-Haskell side ('emanoteStaticUrl') is the same primitive,
called directly from "Emanote.View.JsBundle" for the importmap.
-}
module Emanote.View.StaticUrl (
  emanoteStaticUrl,
  emanoteStaticUrlSplice,
) where

import Data.Map.Syntax ((##))
import Emanote.Model.Type (Model)
import Emanote.Model.Type qualified as M
import Emanote.Route.SiteRoute.Class qualified as SR
import Heist qualified as H
import Heist.Interpreted qualified as HI
import Relude
import System.FilePath ((</>))
import Text.XmlHtml qualified as X

staticDir :: FilePath
staticDir = "_emanote-static"

-- | Cache-busted URL for an asset under @_emanote-static\/\<relPath\>@.
emanoteStaticUrl :: (HasCallStack) => Model -> FilePath -> Text
emanoteStaticUrl model relPath =
  SR.siteRouteUrl model
    $ SR.staticFileSiteRoute
    $ fromMaybe (error . toText $ "no " <> staticDir <> "/" <> relPath <> "?")
    $ M.modelLookupStaticFile (staticDir </> relPath) model

emanoteStaticUrlSplice :: Model -> HI.Splice Identity
emanoteStaticUrlSplice model = do
  node <- H.getParamNode
  let path =
        fromMaybe (error "<emanoteStaticUrl> splice missing 'path' attribute")
          $ X.getAttribute "path" node
  HI.runChildrenWithText ("url" ## emanoteStaticUrl model (toString path))
