{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.View.SiteRoute
  ( SiteRoute (..),
    decodeNonResourceRoute,
    noteFileSiteRoute,
    staticFileSiteRoute,
  )
where

import Control.Lens.Operators ((^.))
import qualified Data.IxSet.Typed as Ix
import Ema (Ema (..))
import Emanote.Model (Model)
import qualified Emanote.Model as M
import qualified Emanote.Model.Note as N
import qualified Emanote.Model.StaticFile as SF
import Emanote.Route (FileType (AnyExt, Html), LinkableLMLRoute, R)
import qualified Emanote.Route as R

-- | Route representing the pages/URLs in the generated website.
--
-- TODO: Use OpenUnion after representing SRIndex and SRTagIndex as special
-- routes. See comment in `UnresolvedRelTarget` type.
data SiteRoute
  = -- | Route to a special note-index view
    SRIndex
  | -- | Route to tag index
    SRTagIndex
  | -- | Route to a LML file that gets generated as HTML
    SRLMLFile LinkableLMLRoute
  | -- | Route to a static file, along with its absolute path on disk
    SRStaticFile (R 'AnyExt, FilePath)
  | -- | A link that points to nowhere in model. Used in live-server mainly.
    SR404 FilePath
  deriving (Eq, Show, Ord)

instance Ema Model SiteRoute where
  encodeRoute model = \case
    SRIndex ->
      R.encodeRoute $
        R.mkRouteFromSlug @'Html "@index"
    SRTagIndex ->
      R.encodeRoute $
        R.mkRouteFromSlug @'Html "@tags"
    SRLMLFile r ->
      R.encodeRoute $
        maybe (coerce . R.linkableLMLRouteCase $ r) N.noteHtmlRoute $
          M.modelLookupNoteByRoute r model
    SRStaticFile (r, _fpAbs) ->
      R.encodeRoute r
    SR404 _fp ->
      error "emanote: attempt to encode a 404 route"

  decodeRoute model fp =
    decodeNonResourceRoute fp
      <|> decodeResourceRoute model fp
      <|> pure (SR404 fp)

  allRoutes model =
    let htmlRoutes =
          model ^. M.modelNotes
            & Ix.toList
            <&> noteFileSiteRoute
        staticRoutes =
          model ^. M.modelStaticFiles
            & Ix.toList
            <&> staticFileSiteRoute
     in htmlRoutes <> staticRoutes <> [SRIndex, SRTagIndex]

-- | Decode a route that does not correspond to a resource in `Model`
decodeNonResourceRoute :: FilePath -> Maybe SiteRoute
decodeNonResourceRoute fp =
  (SRIndex <$ ((guard . (== "@index")) <=< R.routeSlug . R.decodeHtmlRoute $ fp))
    <|> (SRTagIndex <$ ((guard . (== "@tags")) <=< R.routeSlug . R.decodeHtmlRoute $ fp))

decodeResourceRoute :: Model -> FilePath -> Maybe SiteRoute
decodeResourceRoute model fp =
  fmap
    staticFileSiteRoute
    (flip M.modelLookupStaticFileByRoute model =<< R.decodeAnyRoute fp)
    <|> fmap
      noteFileSiteRoute
      (flip M.modelLookupNoteByHtmlRoute model $ R.decodeHtmlRoute fp)

noteFileSiteRoute :: N.Note -> SiteRoute
noteFileSiteRoute =
  SRLMLFile . N._noteRoute

staticFileSiteRoute :: SF.StaticFile -> SiteRoute
staticFileSiteRoute =
  SRStaticFile . (SF._staticFileRoute &&& SF._staticFilePath)
