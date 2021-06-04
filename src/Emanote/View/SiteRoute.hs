{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.View.SiteRoute
  ( SiteRoute (..),
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
data SiteRoute
  = -- | Route to a special note-index view
    SRIndex
  | -- | Route to a LML file that gets generated as HTML
    SRLMLFile LinkableLMLRoute
  | -- | Route to a static file, along with its absolute path on disk
    SRStaticFile (R 'AnyExt, FilePath)
  deriving (Eq, Show, Ord)

instance Ema Model SiteRoute where
  encodeRoute model = \case
    SRIndex ->
      R.encodeRoute $
        R.mkRouteFromSlug @'Html "@index"
    SRLMLFile r ->
      R.encodeRoute $
        maybe (coerce . R.linkableLMLRouteCase $ r) N.noteHtmlRoute $
          M.modelLookupNoteByRoute r model
    SRStaticFile (r, _fpAbs) ->
      R.encodeRoute r

  decodeRoute model fp =
    ( SRIndex
        <$ ((guard . (== "@index")) <=< R.routeSlug <=< R.decodeHtmlRoute $ fp)
    )
      <|> fmap
        staticFileSiteRoute
        (flip M.modelLookupStaticFileByRoute model =<< R.decodeAnyRoute fp)
      <|> fmap
        noteFileSiteRoute
        (flip M.modelLookupNoteByHtmlRoute model =<< R.decodeHtmlRoute fp)

  allRoutes model =
    let htmlRoutes =
          model ^. M.modelNotes
            & Ix.toList
            <&> noteFileSiteRoute
        staticRoutes =
          model ^. M.modelStaticFiles
            & Ix.toList
            <&> staticFileSiteRoute
     in htmlRoutes <> staticRoutes <> [SRIndex]

noteFileSiteRoute :: N.Note -> SiteRoute
noteFileSiteRoute =
  SRLMLFile . N._noteRoute

staticFileSiteRoute :: SF.StaticFile -> SiteRoute
staticFileSiteRoute =
  SRStaticFile . (SF._staticFileRoute &&& SF._staticFilePath)
