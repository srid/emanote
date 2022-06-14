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
import Emanote.Route.Ext (FileType (AnyExt, LMLType), LML (Md))
import Emanote.Route.R (R)
import Emanote.Route.R qualified as R
import Relude

type StaticFileRoute = R 'AnyExt

-- | A R to anywhere in `Model`
data ModelRoute
  = ModelRoute_StaticFile StaticFileRoute
  | ModelRoute_LML LMLRoute
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (ToJSON)

-- | R to a note file in LML (lightweight markup language) format
newtype LMLRoute
  = LMLRoute_Md (R ('LMLType 'Md))
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (ToJSON)

-- TODO: Revamp this, and make it work with .org, etc.
liftLMLRoute ::
  R ('LMLType 'Md) ->
  LMLRoute
liftLMLRoute =
  LMLRoute_Md

lmlRouteCase ::
  LMLRoute ->
  R ('LMLType 'Md)
lmlRouteCase = \case
  LMLRoute_Md r -> r

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
