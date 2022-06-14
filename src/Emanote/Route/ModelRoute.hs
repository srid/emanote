{-# LANGUAGE DeriveAnyClass #-}

-- | Route types representing the resources in our `Model`.
--
-- See also: `Emanote.Route.SiteRoute`.
module Emanote.Route.ModelRoute
  ( -- Some route in a generated site
    ModelRoute (..),
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

import Data.Aeson.Types (ToJSON)
import Data.WorldPeace.Union
  ( IsMember,
    OpenUnion,
    absurdUnion,
    openUnionHandle,
    openUnionLift,
  )
import Emanote.Route.Ext (FileType (AnyExt, LMLType), LML (Md), SourceExt)
import Emanote.Route.R (R)
import Emanote.Route.R qualified as R
import Relude

type LMLRoutes' =
  '[ R ('LMLType 'Md)
   ]

type StaticFileRoute = R 'AnyExt

-- | A R to anywhere in `Model`
data ModelRoute
  = ModelRoute_StaticFile StaticFileRoute
  | ModelRoute_LML LMLRoute
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (ToJSON)

-- | R to a note file in LML (lightweight markup language) format
type LMLRoute = OpenUnion LMLRoutes'

liftLMLRoute ::
  forall ext.
  IsMember (R ext) LMLRoutes' =>
  R (ext :: FileType SourceExt) ->
  LMLRoute
liftLMLRoute =
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
modelRouteCase = \case
  ModelRoute_LML r -> Left r
  ModelRoute_StaticFile r -> Right r

mkModelRouteFromFilePath :: FilePath -> Maybe ModelRoute
mkModelRouteFromFilePath fp =
  fmap (ModelRoute_LML . liftLMLRoute) (R.mkRouteFromFilePath @_ @('LMLType 'Md) fp)
    <|> fmap ModelRoute_StaticFile (R.mkRouteFromFilePath @_ @'AnyExt fp)
