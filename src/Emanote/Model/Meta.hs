{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Emanote.Model.Meta where

import Control.Lens.Operators as Lens ((^.))
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.IxSet.Typed as Ix
import Emanote.Model (Model, modelLookupNote, modelSData)
import Emanote.Model.Note (lookupAeson, noteMeta)
import Emanote.Model.SData (sdataValue)
import qualified Emanote.Model.SData as SData
import qualified Emanote.Route as R

-- | Look up a specific key in the meta for a given route.
lookupMeta :: FromJSON a => a -> NonEmpty Text -> R.LinkableLMLRoute -> Model -> a
lookupMeta x k r =
  lookupAeson x k . getEffectiveRouteMeta r

-- | Get the (final) metadata of a note at the given route, by merging it with
-- the defaults specified in parent routes all the way upto index.yaml.
getEffectiveRouteMeta :: R.LinkableLMLRoute -> Model -> Aeson.Value
getEffectiveRouteMeta mr model =
  let defaultFiles = R.routeInits @'R.Yaml (coerce $ R.someLinkableLMLRouteCase mr)
      defaults = flip mapMaybe (toList defaultFiles) $ \r -> do
        v <- fmap (^. sdataValue) . Ix.getOne . Ix.getEQ r $ model ^. modelSData
        guard $ v /= Aeson.Null
        pure v
      frontmatter = do
        x <- (^. noteMeta) <$> modelLookupNote mr model
        guard $ x /= Aeson.Null
        pure x
      metas = defaults <> maybe mempty one frontmatter
   in maybe Aeson.Null SData.mergeAesons $ nonEmpty metas
