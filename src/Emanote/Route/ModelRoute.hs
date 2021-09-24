{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Route types representing the resources in our `Model`.
--
-- See also: `Emanote.Route.SiteRoute`.
module Emanote.Route.ModelRoute
  ( -- Some route in a generated site
    ModelRoute,
    liftModelRoute,
    modelRouteCase,
    mkModelRouteFromFilePath,
    -- Only LML routes
    LMLRoute,
    liftLMLRoute,
    lmlRouteCase,
    -- Static file routes
    StaticFileRoute,
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
import Relude

type LMLRoutes' =
  '[ R ('LMLType 'Md)
   ]

type StaticFileRoute = R 'AnyExt

-- | A "route" into the `Model`.
type ModelRoutes' =
  StaticFileRoute
    ': LMLRoutes'

-- | A R to anywhere in `Model`
type ModelRoute = OpenUnion ModelRoutes'

-- | R to a note file in LML (lightweight markup language) format
type LMLRoute = OpenUnion LMLRoutes'

liftLMLRoute ::
  forall ext.
  IsMember (R ext) LMLRoutes' =>
  R (ext :: FileType) ->
  LMLRoute
liftLMLRoute =
  openUnionLift

liftModelRoute ::
  IsMember (R ext) ModelRoutes' =>
  R (ext :: FileType) ->
  ModelRoute
liftModelRoute =
  openUnionLift

lmlRouteCase ::
  LMLRoute ->
  R ('LMLType 'Md)
lmlRouteCase =
  absurdUnion
    `openUnionHandle` id

modelRouteCase ::
  ModelRoute ->
  Either LMLRoute StaticFileRoute
modelRouteCase =
  first (liftLMLRoute @('LMLType 'Md))
    . ( absurdUnion
          `openUnionHandle` Left
          `openUnionHandle` Right
      )

mkModelRouteFromFilePath :: FilePath -> Maybe ModelRoute
mkModelRouteFromFilePath fp =
  fmap liftModelRoute (R.mkRouteFromFilePath @('LMLType 'Md) fp)
    <|> fmap liftModelRoute (R.mkRouteFromFilePath @'AnyExt fp)
