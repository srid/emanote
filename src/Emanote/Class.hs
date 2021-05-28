{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Emanote.Class where

import Control.Lens.Operators ((^.))
import qualified Data.IxSet.Typed as Ix
import Ema (Ema (..))
import Emanote.Model (Model)
import qualified Emanote.Model as M
import qualified Emanote.Model.Note as N
import qualified Emanote.Model.StaticFile as SF
import Emanote.Route (Route)
import qualified Emanote.Route as R
import Emanote.Route.Ext (FileType (AnyExt, Html))
import Emanote.Route.Linkable (LinkableLMLRoute, someLinkableLMLRouteCase)

data EmanoteRoute
  = ERNoteHtml (Route 'Html)
  | EROtherFile
      ( Route 'AnyExt,
        -- Absolute path to the static file referenced by this route
        FilePath
      )
  deriving (Eq, Show, Ord)

instance Ema Model EmanoteRoute where
  encodeRoute = \case
    ERNoteHtml r ->
      R.encodeRoute r
    EROtherFile (r, _fpAbs) ->
      R.encodeRoute r

  decodeRoute model fp =
    fmap staticFileRoute (M.modelLookupStaticFile fp model)
      <|> fmap ERNoteHtml (R.decodeHtmlRoute fp)

  allRoutes model =
    let htmlRoutes =
          model ^. M.modelNotes
            & Ix.toList
            <&> lmlHtmlRoute . (^. N.noteRoute)
        staticRoutes =
          model ^. M.modelStaticFiles
            & Ix.toList
            <&> staticFileRoute
     in htmlRoutes <> staticRoutes

lmlHtmlRoute :: LinkableLMLRoute -> EmanoteRoute
lmlHtmlRoute =
  ERNoteHtml . htmlRouteForLmlRoute
  where
    htmlRouteForLmlRoute :: LinkableLMLRoute -> Route 'Html
    htmlRouteForLmlRoute = coerce . someLinkableLMLRouteCase

staticFileRoute :: SF.StaticFile -> EmanoteRoute
staticFileRoute =
  EROtherFile . (SF._staticFileRoute &&& SF._staticFilePath)
