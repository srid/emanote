{-# LANGUAGE DeriveAnyClass #-}

{- | Route types representing the resources in our `Model`.

 See also: `Emanote.Route.SiteRoute`.
-}
module Emanote.Route.ModelRoute (
  -- Some route in a generated site
  ModelRoute (..),
  modelRouteCase,
  mkModelRouteCandidates,
  encodeModelRoute,
  -- Only LML routes
  LMLView (..),
  LMLRoute (..),
  defaultLmlRoute,
  possibleLmlRoutes,
  lmlRouteCase,
  withLmlRoute,
  mkLMLRouteFromFilePath,
  mkLMLRouteFromKnownFilePath,
  isMdRoute,
  -- Static file routes
  StaticFileRoute,
) where

import Data.Aeson.Types (ToJSON)
import Emanote.Route.Ext (FileType (AnyExt, LMLType, Xml), HasExt, LML (Md, Org))
import Emanote.Route.R (R)
import Emanote.Route.R qualified as R
import Relude

type StaticFileRoute = R 'AnyExt

data LMLView = LMLView_Html | LMLView_Atom
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData, ToJSON)

-- | A R to anywhere in `Model`
data ModelRoute
  = ModelRoute_StaticFile StaticFileRoute
  | ModelRoute_LML LMLView LMLRoute
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData, ToJSON)

-- | R to a note file in LML (lightweight markup language) format
data LMLRoute
  = LMLRoute_Md (R ('LMLType 'Md))
  | LMLRoute_Org (R ('LMLType 'Org))
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData, ToJSON)

defaultLmlRoute :: R (ext :: FileType a) -> LMLRoute
defaultLmlRoute =
  LMLRoute_Md . coerce

possibleLmlRoutes :: R (ext :: FileType a) -> [LMLRoute]
possibleLmlRoutes r =
  [ LMLRoute_Md (coerce r)
  , LMLRoute_Org (coerce r)
  ]

lmlRouteCase ::
  LMLRoute ->
  Either (R ('LMLType 'Md)) (R ('LMLType 'Org))
lmlRouteCase = \case
  LMLRoute_Md r -> Left r
  LMLRoute_Org r -> Right r

isMdRoute :: LMLRoute -> Bool
isMdRoute = \case
  LMLRoute_Md _ -> True
  _ -> False

withLmlRoute :: (forall lmlType. (HasExt ('LMLType lmlType)) => R ('LMLType lmlType) -> r) -> LMLRoute -> r
withLmlRoute f = either f f . lmlRouteCase

modelRouteCase ::
  ModelRoute ->
  Either (LMLView, LMLRoute) StaticFileRoute
modelRouteCase = \case
  ModelRoute_LML view r -> Left (view, r)
  ModelRoute_StaticFile r -> Right r

{- | All `ModelRoute`s a URL filepath could plausibly resolve to,
ordered by preference.

The same URL can map to multiple resource kinds — most importantly,
@*.xml@ is both the natural URL for a feed-enabled note's Atom output
and the natural URL for a static @.xml@ asset. Resolution at lookup
time picks the first candidate that exists in the model; if the user
intended one but the model only contains the other, the link still
resolves correctly.
-}
mkModelRouteCandidates :: FilePath -> [ModelRoute]
mkModelRouteCandidates fp =
  maybeToList (uncurry ModelRoute_LML <$> mkLMLRouteFromFilePath fp)
    <> maybeToList (ModelRoute_StaticFile <$> R.mkRouteFromFilePath fp)

-- | Encode a `ModelRoute` to its on-the-wire URL filepath.
encodeModelRoute :: ModelRoute -> FilePath
encodeModelRoute = \case
  ModelRoute_StaticFile r -> R.encodeRoute r
  ModelRoute_LML LMLView_Html lmlR -> withLmlRoute R.encodeRoute lmlR
  ModelRoute_LML LMLView_Atom lmlR -> R.encodeRoute @_ @'Xml (withLmlRoute coerce lmlR)

mkLMLRouteFromFilePath :: FilePath -> Maybe (LMLView, LMLRoute)
mkLMLRouteFromFilePath fp =
  fmap
    (LMLView_Html,)
    ( mkLMLRouteFromKnownFilePath Md fp
        <|> mkLMLRouteFromKnownFilePath Org fp
    )
    <|> ( do
            xmlR <- R.mkRouteFromFilePath @_ @'Xml fp
            pure (LMLView_Atom, LMLRoute_Md $ coerce xmlR)
        )

{- | Like `mkLMLRouteFromFilePath`, but when the file extension is known ahead
 to be of `lmlType`.
-}
mkLMLRouteFromKnownFilePath :: LML -> FilePath -> Maybe LMLRoute
mkLMLRouteFromKnownFilePath lmlType fp =
  case lmlType of
    Md -> fmap LMLRoute_Md (R.mkRouteFromFilePath' True fp)
    Org -> fmap LMLRoute_Org (R.mkRouteFromFilePath' True fp)
