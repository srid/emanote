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
    LMLRoute (..),
    lmlRouteCase,
    withLmlRoute,
    mkLMLRouteFromFilePath,
    -- Static file routes
    StaticFileRoute,
  )
where

import Data.Aeson.Types (ToJSON)
import Emanote.Route.Ext (FileType (AnyExt, LMLType), HasExt, LML (Md, Org))
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
data LMLRoute
  = LMLRoute_Md (R ('LMLType 'Md))
  | LMLRoute_Org (R ('LMLType 'Org))
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (ToJSON)

lmlRouteCase ::
  LMLRoute ->
  Either (R ('LMLType 'Md)) (R ('LMLType 'Org))
lmlRouteCase = \case
  LMLRoute_Md r -> Left r
  LMLRoute_Org r -> Right r

withLmlRoute :: (forall lmlType. HasExt ('LMLType lmlType) => R ('LMLType lmlType) -> r) -> LMLRoute -> r
withLmlRoute f = either f f . lmlRouteCase

modelRouteCase ::
  ModelRoute ->
  Either LMLRoute StaticFileRoute
modelRouteCase = \case
  ModelRoute_LML r -> Left r
  ModelRoute_StaticFile r -> Right r

mkModelRouteFromFilePath :: FilePath -> Maybe ModelRoute
mkModelRouteFromFilePath fp =
  fmap ModelRoute_LML (mkLMLRouteFromFilePath fp)
    <|> fmap ModelRoute_StaticFile (R.mkRouteFromFilePath @_ @'AnyExt fp)

mkLMLRouteFromFilePath :: FilePath -> Maybe LMLRoute
mkLMLRouteFromFilePath fp =
  fmap LMLRoute_Md (R.mkRouteFromFilePath fp)
    <|> fmap LMLRoute_Org (R.mkRouteFromFilePath fp)
