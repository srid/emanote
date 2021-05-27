{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

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

type Routes =
  '[ Route ('LMLType 'Md),
     Route 'AnyExt
   ]

type SomeRoute = OpenUnion Routes

liftSomeRoute ::
  IsMember (Route ext) Routes =>
  Route (ext :: FileType) ->
  SomeRoute
liftSomeRoute =
  openUnionLift

someRouteMatch ::
  IsMember (Route ext) Routes =>
  OpenUnion Routes ->
  Maybe (Route ext)
someRouteMatch =
  openUnionMatch

someRouteCase ::
  SomeRoute ->
  Either (Route ('LMLType 'Md)) (Route 'AnyExt)
someRouteCase =
  absurdUnion
    `openUnionHandle` Left
    `openUnionHandle` Right
