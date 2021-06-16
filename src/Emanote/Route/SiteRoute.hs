{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Emanote.Route.SiteRoute
  ( SiteRoute,
  )
where

import Data.WorldPeace.Union
import Emanote.Route.ModelRoute

data IndexR = IndexR

data TagIndexR = TagIndexR

-- | A 404 route
newtype MissingR = MissingR {unMissingR :: FilePath}
  deriving (Eq, Show, Ord)

type SiteRoute' =
  '[ IndexR,
     TagIndexR,
     ModelRoute,
     MissingR
   ]

type SiteRoute = OpenUnion SiteRoute'
