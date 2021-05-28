{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Emanote.Route.Linkable
  ( -- Some route in a generated site
    LinkableRoute,
    liftLinkableRoute,
    linkableRouteCase,
    mkLinkableLMLRouteFromFilePath,
    -- Only LML routes
    LinkableLMLRoute,
    liftLinkableLMLRoute,
    someLinkableLMLRouteCase,
  )
where

import Data.WorldPeace.Union
  ( IsMember,
    OpenUnion,
    absurdUnion,
    openUnionHandle,
    openUnionLift,
  )
import Emanote.Route (Route)
import qualified Emanote.Route as R
import Emanote.Route.Ext (FileType (AnyExt, LMLType), LML (Md))

type LMLRoutes =
  '[ Route ('LMLType 'Md)
   ]

type Routes =
  Route 'AnyExt
    ': LMLRoutes

-- | A Route that can be linked to from anywhere.
type LinkableRoute = OpenUnion Routes

-- | Route to a note file in LML (lightweight markup language) format
type LinkableLMLRoute = OpenUnion LMLRoutes

liftLinkableLMLRoute ::
  IsMember (Route ext) LMLRoutes =>
  Route (ext :: FileType) ->
  LinkableLMLRoute
liftLinkableLMLRoute =
  openUnionLift

liftLinkableRoute ::
  IsMember (Route ext) Routes =>
  Route (ext :: FileType) ->
  LinkableRoute
liftLinkableRoute =
  openUnionLift

someLinkableLMLRouteCase ::
  LinkableLMLRoute ->
  Route ('LMLType 'Md)
someLinkableLMLRouteCase =
  absurdUnion
    `openUnionHandle` id

linkableRouteCase ::
  LinkableRoute ->
  Either LinkableLMLRoute (Route 'AnyExt)
linkableRouteCase =
  first (liftLinkableLMLRoute @('LMLType 'Md))
    . ( absurdUnion
          `openUnionHandle` Left
          `openUnionHandle` Right
      )

mkLinkableLMLRouteFromFilePath :: FilePath -> Maybe LinkableLMLRoute
mkLinkableLMLRouteFromFilePath fp =
  fmap liftLinkableLMLRoute (R.mkRouteFromFilePath @('LMLType 'Md) fp)
