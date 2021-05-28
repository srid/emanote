{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Linkable route types
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
import Emanote.Route.Ext (FileType (AnyExt, LMLType), LML (Md))
import Emanote.Route.R (R)
import qualified Emanote.Route.R as R

type LMLRoutes =
  '[ R ('LMLType 'Md)
   ]

type Routes =
  R 'AnyExt
    ': LMLRoutes

-- | A R that can be linked to from anywhere.
type LinkableRoute = OpenUnion Routes

-- | R to a note file in LML (lightweight markup language) format
type LinkableLMLRoute = OpenUnion LMLRoutes

liftLinkableLMLRoute ::
  IsMember (R ext) LMLRoutes =>
  R (ext :: FileType) ->
  LinkableLMLRoute
liftLinkableLMLRoute =
  openUnionLift

liftLinkableRoute ::
  IsMember (R ext) Routes =>
  R (ext :: FileType) ->
  LinkableRoute
liftLinkableRoute =
  openUnionLift

someLinkableLMLRouteCase ::
  LinkableLMLRoute ->
  R ('LMLType 'Md)
someLinkableLMLRouteCase =
  absurdUnion
    `openUnionHandle` id

linkableRouteCase ::
  LinkableRoute ->
  Either LinkableLMLRoute (R 'AnyExt)
linkableRouteCase =
  first (liftLinkableLMLRoute @('LMLType 'Md))
    . ( absurdUnion
          `openUnionHandle` Left
          `openUnionHandle` Right
      )

mkLinkableLMLRouteFromFilePath :: FilePath -> Maybe LinkableLMLRoute
mkLinkableLMLRouteFromFilePath fp =
  fmap liftLinkableLMLRoute (R.mkRouteFromFilePath @('LMLType 'Md) fp)
