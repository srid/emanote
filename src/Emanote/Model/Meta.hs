module Emanote.Model.Meta
  ( lookupRouteMeta,
    getEffectiveRouteMeta,
    getEffectiveRouteMetaWith,
  )
where

import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.IxSet.Typed qualified as Ix
import Emanote.Model (ModelT, modelLookupNoteByRoute, modelSData)
import Emanote.Model.Note (_noteMeta)
import Emanote.Model.SData (sdataValue)
import Emanote.Model.SData qualified as SData
import Emanote.Route qualified as R
import Optics.Operators as Lens ((^.))
import Relude

-- | Look up a specific key in the meta for a given route.
lookupRouteMeta :: FromJSON a => a -> NonEmpty Text -> R.LMLRoute -> ModelT f -> a
lookupRouteMeta x k r =
  SData.lookupAeson x k . getEffectiveRouteMeta r

-- | Get the (final) metadata of a note at the given route, by merging it with
-- the defaults specified in parent routes all the way upto index.yaml.
getEffectiveRouteMeta :: R.LMLRoute -> ModelT f -> Aeson.Value
getEffectiveRouteMeta mr model =
  let mNote = modelLookupNoteByRoute mr model
   in getEffectiveRouteMetaWith (maybe Aeson.Null _noteMeta mNote) mr model

getEffectiveRouteMetaWith :: Aeson.Value -> R.LMLRoute -> ModelT f -> Aeson.Value
getEffectiveRouteMetaWith frontmatter mr model =
  let defaultFiles = R.routeInits @'R.Yaml (R.withLmlRoute coerce mr)
      defaults = flip mapMaybe (toList defaultFiles) $ \r -> do
        v <- getYamlMeta r model
        guard $ v /= Aeson.Null
        pure v
      metas = defaults <> maybe mempty one (guard (frontmatter /= Aeson.Null) >> pure frontmatter)
   in maybe Aeson.Null SData.mergeAesons $ nonEmpty metas

getYamlMeta :: R.R 'R.Yaml -> ModelT f -> Maybe Aeson.Value
getYamlMeta r model =
  fmap (^. sdataValue) . Ix.getOne . Ix.getEQ r $ model ^. modelSData
