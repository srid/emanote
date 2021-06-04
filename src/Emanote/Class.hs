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
import Emanote.Route (FileType (AnyExt, Html), LinkableLMLRoute, R)
import qualified Emanote.Route as R

-- | Route to something or some view in the `Model`, that we also want generated in static site.
data Route
  = -- | Route to a special note-index view
    RIndex
  | -- | Route to a LML file that gets generated as HTML
    RLMLFile LinkableLMLRoute
  | -- | Route to a static file, along with its absolute path on disk
    RStaticFile (R 'AnyExt, FilePath)
  deriving (Eq, Show, Ord)

instance Ema Model Route where
  encodeRoute model = \case
    RIndex ->
      R.encodeRoute $ R.mkRouteFromSlug @'Html "@index"
    RLMLFile r ->
      R.encodeRoute $
        fromMaybe (coerce . R.someLinkableLMLRouteCase $ r) $ do
          note <- M.modelLookupNote r model
          pure $ N.noteHtmlRoute note
    RStaticFile (r, _fpAbs) ->
      R.encodeRoute r

  decodeRoute model fp =
    (RIndex <$ (R.decodeHtmlRoute fp >>= \r -> guard $ r == R.mkRouteFromSlug "@index"))
      <|> fmap staticFileRoute (M.modelLookupStaticFile fp model)
      <|> fmap
        (RLMLFile . N._noteRoute)
        (flip N.lookupNoteOrItsParent (model ^. M.modelNotes) =<< R.decodeHtmlRoute fp)

  allRoutes model =
    let htmlRoutes =
          model ^. M.modelNotes
            & Ix.toList
            <&> RLMLFile . (^. N.noteRoute)
        staticRoutes =
          model ^. M.modelStaticFiles
            & Ix.toList
            <&> staticFileRoute
     in htmlRoutes <> staticRoutes <> [RIndex]

staticFileRoute :: SF.StaticFile -> Route
staticFileRoute =
  RStaticFile . (SF._staticFileRoute &&& SF._staticFilePath)
