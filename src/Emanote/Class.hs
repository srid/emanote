{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Emanote.Class where

import Control.Lens.Operators ((^.))
import qualified Data.IxSet.Typed as Ix
import qualified Data.Map.Strict as Map
import Ema (Ema (..))
import Emanote.Model (Model)
import qualified Emanote.Model as M
import qualified Emanote.Model.Note as N
import Emanote.Route (Route)
import qualified Emanote.Route as R
import Emanote.Route.Ext (FileType (AnyExt, Html))
import Emanote.Route.SomeRoute (SomeLMLRoute, someLMLRouteCase)

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
    fmap EROtherFile (M.modelLookupStaticFile fp model)
      <|> fmap ERNoteHtml (R.decodeHtmlRoute fp)

  allRoutes model =
    let htmlRoutes =
          model ^. M.modelNotes
            & Ix.toList
            <&> htmlRouteForLmlRoute . (^. N.noteRoute)
        staticFiles =
          Map.toList $ model ^. M.modelStaticFiles
     in fmap ERNoteHtml htmlRoutes
          <> fmap EROtherFile staticFiles

htmlRouteForLmlRoute :: SomeLMLRoute -> Route 'Html
htmlRouteForLmlRoute = coerce . someLMLRouteCase
