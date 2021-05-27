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
import qualified Emanote.Route as R
import Emanote.Route.Ext (FileType (AnyExt, LMLType), LML (Md))

type LMLRoutes =
  '[ Route ('LMLType 'Md)
   ]

-- NOTE: We are not including .yaml and .tpl routes, as they are unused (yet).
type Routes =
  Route 'AnyExt
    ': LMLRoutes

-- | Route to anything
type SomeRoute = OpenUnion Routes

-- | Route to a note file in LML (lightweight markup language) format
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

mkLmlRouteFromFilePath :: FilePath -> Maybe SomeLMLRoute
mkLmlRouteFromFilePath fp =
  fmap liftSomeLMLRoute (R.mkRouteFromFilePath @('LMLType 'Md) fp)

mkAnyExtRouteFromFilePath :: HasCallStack => FilePath -> SomeRoute
mkAnyExtRouteFromFilePath fp =
  fromMaybe (error "BUG: AnyExt fallback failed; impossible") $ do
    fmap liftSomeRoute (R.mkRouteFromFilePath @'AnyExt fp)