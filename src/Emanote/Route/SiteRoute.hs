{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Emanote.Route.SiteRoute
  ( SiteRoute,
    IndexR (..),
    TagIndexR (..),
    MissingR (..),
    VirtualRoute,
    decodeVirtualRoute,
    noteFileSiteRoute,
    staticFileSiteRoute,
    lmlSiteRoute,
  )
where

import Control.Lens.Operators ((^.))
import qualified Data.IxSet.Typed as Ix
import Data.WorldPeace.Union
  ( OpenUnion,
    absurdUnion,
    openUnionLift,
  )
import Ema (Ema (..))
import qualified Emanote.Model as M
import qualified Emanote.Model.Note as N
import qualified Emanote.Model.StaticFile as SF
import Emanote.Model.Type (Model)
import Emanote.Prelude (h)
import Emanote.Route (FileType (Html))
import qualified Emanote.Route as R
import Emanote.Route.ModelRoute (LMLRoute, StaticFileRoute)

data IndexR = IndexR
  deriving (Eq, Show, Ord)

data TagIndexR = TagIndexR
  deriving (Eq, Show, Ord)

-- | A 404 route
newtype MissingR = MissingR {unMissingR :: FilePath}
  deriving (Eq, Show, Ord)

-- | A route to a virtual resource (not in `Model`)
type VirtualRoute' =
  '[ IndexR,
     TagIndexR
   ]

type VirtualRoute = OpenUnion VirtualRoute'

type SiteRoute' =
  '[ VirtualRoute,
     (StaticFileRoute, FilePath),
     LMLRoute,
     MissingR
   ]

type SiteRoute = OpenUnion SiteRoute'

instance Ema Model SiteRoute where
  encodeRoute model =
    absurdUnion
      `h` ( \(MissingR _fp) ->
              error "emanote: attempt to encode a 404 route"
          )
      `h` ( \(r :: LMLRoute) ->
              R.encodeRoute $
                maybe (coerce . R.lmlRouteCase $ r) N.noteHtmlRoute $
                  M.modelLookupNoteByRoute r model
          )
      `h` ( \(r :: StaticFileRoute, _fpAbs :: FilePath) ->
              R.encodeRoute r
          )
      `h` encodeVirtualRoute

  decodeRoute model fp =
    fmap openUnionLift (decodeVirtualRoute fp)
      <|> decodeGeneratedRoute model fp
      <|> pure (openUnionLift $ MissingR fp)
  allRoutes model =
    let htmlRoutes =
          model ^. M.modelNotes
            & Ix.toList
            <&> noteFileSiteRoute
        staticRoutes =
          model ^. M.modelStaticFiles
            & Ix.toList
            <&> staticFileSiteRoute
        virtualRoutes :: [VirtualRoute] =
          [openUnionLift IndexR, openUnionLift TagIndexR]
     in htmlRoutes
          <> staticRoutes
          <> fmap openUnionLift virtualRoutes

encodeVirtualRoute :: VirtualRoute -> FilePath
encodeVirtualRoute =
  absurdUnion
    `h` ( \TagIndexR ->
            R.encodeRoute $ R.mkRouteFromSlug @'Html "@tags"
        )
    `h` ( \IndexR ->
            R.encodeRoute $ R.mkRouteFromSlug @'Html "@index"
        )

-- | Decode a route that is known to refer to a resource in the model
decodeGeneratedRoute :: Model -> FilePath -> Maybe SiteRoute
decodeGeneratedRoute model fp =
  fmap
    staticFileSiteRoute
    (flip M.modelLookupStaticFileByRoute model =<< R.decodeAnyRoute fp)
    <|> fmap
      noteFileSiteRoute
      (flip M.modelLookupNoteByHtmlRoute model $ R.decodeHtmlRoute fp)

decodeVirtualRoute :: FilePath -> Maybe VirtualRoute
decodeVirtualRoute fp =
  fmap openUnionLift (decodeIndexR fp)
    <|> fmap openUnionLift (decodeTagIndexR fp)

decodeIndexR :: FilePath -> Maybe IndexR
decodeIndexR fp = do
  slug <- R.routeSlug . R.decodeHtmlRoute $ fp
  guard $ slug == "@index"
  pure IndexR

decodeTagIndexR :: FilePath -> Maybe TagIndexR
decodeTagIndexR fp = do
  slug <- R.routeSlug . R.decodeHtmlRoute $ fp
  guard $ slug == "@tags"
  pure TagIndexR

noteFileSiteRoute :: N.Note -> SiteRoute
noteFileSiteRoute =
  lmlSiteRoute . N._noteRoute

lmlSiteRoute :: LMLRoute -> SiteRoute
lmlSiteRoute =
  openUnionLift

staticFileSiteRoute :: SF.StaticFile -> SiteRoute
staticFileSiteRoute =
  openUnionLift . (SF._staticFileRoute &&& SF._staticFilePath)