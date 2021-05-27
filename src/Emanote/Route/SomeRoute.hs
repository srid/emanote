{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Emanote.Route.SomeRoute where

import Data.WorldPeace.Union
  ( IsMember,
    OpenUnion,
    absurdUnion,
    openUnionHandle,
    openUnionLift,
    openUnionMatch,
  )
import Emanote.Route (Route)
import Emanote.Route.Ext (FileType (AnyExt, LMLType), LML (Md))

type LMLRoutes =
  '[ Route ('LMLType 'Md)
   ]

type Routes =
  Route 'AnyExt
    ': LMLRoutes

-- | Route to anything
type SomeRoute = OpenUnion Routes

-- | Route to a note file
type SomeLMLRoute = OpenUnion LMLRoutes

liftSomeLMLRoute ::
  IsMember (Route ext) LMLRoutes =>
  Route (ext :: FileType) ->
  SomeLMLRoute
liftSomeLMLRoute =
  openUnionLift

liftSomeRoute ::
  IsMember (Route ext) Routes =>
  Route (ext :: FileType) ->
  SomeRoute
liftSomeRoute =
  openUnionLift

someLMLRouteCase ::
  SomeLMLRoute ->
  Route ('LMLType 'Md)
someLMLRouteCase =
  absurdUnion
    `openUnionHandle` id

someRouteMatch ::
  IsMember (Route ext) Routes =>
  OpenUnion Routes ->
  Maybe (Route ext)
someRouteMatch =
  openUnionMatch

someRouteCase ::
  SomeRoute ->
  Either SomeLMLRoute (Route 'AnyExt)
someRouteCase =
  first (liftSomeLMLRoute @('LMLType 'Md))
    . ( absurdUnion
          `openUnionHandle` Left
          `openUnionHandle` Right
      )
