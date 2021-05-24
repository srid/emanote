{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Emanote.Class where

import Control.Lens.Operators ((^.))
import qualified Data.IxSet.Typed as Ix
import qualified Data.Set as Set
import Ema (Ema (..))
import Emanote.Model (Model)
import qualified Emanote.Model as M
import qualified Emanote.Model.Note as N
import Emanote.Route (Route)
import qualified Emanote.Route as R
import Emanote.Route.Ext (FileType (AnyExt, Html, LMLType))

-- | TODO: Use `OpenUnion` here?
data EmanoteRoute
  = ERNoteHtml (Route 'Html)
  | EROtherFile (Route 'AnyExt)
  deriving (Eq, Show, Ord)

instance Ema Model EmanoteRoute where
  encodeRoute = \case
    ERNoteHtml r ->
      R.encodeRoute r
    EROtherFile r ->
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
          Set.toList $ model ^. M.modelStaticFiles
     in fmap ERNoteHtml htmlRoutes
          <> fmap EROtherFile staticFiles

htmlRouteForLmlRoute :: Route ('LMLType x) -> Route 'Html
htmlRouteForLmlRoute = coerce
