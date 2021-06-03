{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Emanote.Class where

import Control.Lens.Operators ((^.))
import qualified Data.IxSet.Typed as Ix
import Ema (Ema (..))
import Emanote.Model (Model)
import qualified Emanote.Model as M
import qualified Emanote.Model.Note as N
import qualified Emanote.Model.StaticFile as SF
import Emanote.Route (FileType (AnyExt, Html, LMLType), LML (Md), LinkableLMLRoute, R)
import qualified Emanote.Route as R

data EmanoteRoute
  = ERIndex
  | ERNoteHtml (R 'Html)
  | EROtherFile
      ( R 'AnyExt,
        -- Absolute path to the static file referenced by this route
        FilePath
      )
  deriving (Eq, Show, Ord)

instance Ema Model EmanoteRoute where
  encodeRoute model = \case
    ERIndex ->
      "@index.html"
    ERNoteHtml r ->
      R.encodeRoute $
        fromMaybe r $ do
          note <- M.modelLookupNote (R.liftLinkableLMLRoute @('LMLType 'Md) $ coerce r) model
          pure $ N.noteHtmlRoute note
    EROtherFile (r, _fpAbs) ->
      R.encodeRoute r

  decodeRoute model fp =
    (ERIndex <$ guard (fp == "@index.html" || fp == "@index"))
      <|> fmap staticFileRoute (M.modelLookupStaticFile fp model)
      -- TODO: Lookup model here or actually change the route type.
      -- BUT we do want to account for non-existant folder routes.
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
     in htmlRoutes <> staticRoutes <> [ERIndex]

lmlHtmlRoute :: LinkableLMLRoute -> EmanoteRoute
lmlHtmlRoute =
  ERNoteHtml . htmlRouteForLmlRoute
  where
    htmlRouteForLmlRoute :: LinkableLMLRoute -> R 'Html
    htmlRouteForLmlRoute = coerce . R.someLinkableLMLRouteCase

staticFileRoute :: SF.StaticFile -> EmanoteRoute
staticFileRoute =
  EROtherFile . (SF._staticFileRoute &&& SF._staticFilePath)
