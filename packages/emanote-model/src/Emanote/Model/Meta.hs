module Emanote.Model.Meta (
  lookupRouteMeta,
  getEffectiveRouteMeta,
  getEffectiveRouteMetaWith,
  cascadeYamlErrors,
) where

import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.IxSet.Typed qualified as Ix
import Emanote.Model (ModelT, modelLookupNoteByRoute', modelLookupSData, modelSData)
import Emanote.Model.Note (_noteMeta)
import Emanote.Model.SData (sdataValue)
import Emanote.Model.SData qualified as SData
import Emanote.Route qualified as R
import Optics.Operators as Lens ((^.))
import Relude

-- | Look up a specific key in the meta for a given route.
lookupRouteMeta :: (FromJSON a) => a -> NonEmpty Text -> R.LMLRoute -> ModelT f -> a
lookupRouteMeta x k r =
  SData.lookupAeson x k . getEffectiveRouteMeta r

{- | Get the (final) metadata of a note at the given route, by merging it with
 the defaults specified in parent routes all the way upto index.yaml.
-}
getEffectiveRouteMeta :: R.LMLRoute -> ModelT f -> Aeson.Value
getEffectiveRouteMeta mr model =
  let mNote = modelLookupNoteByRoute' mr model
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
getYamlMeta r model = do
  s <- Ix.getOne . Ix.getEQ r $ model ^. modelSData
  rightToMaybe (s ^. sdataValue)

{- | YAML parse errors whose route is in this LML route's cascade.

A bad @subfolder/index.yaml@ contributes meta to notes under
@\/subfolder\/*@ via 'getEffectiveRouteMetaWith'; this function walks
the same cascade and gathers the parse errors so callers can surface
them on the affected notes (and only those notes).
-}
cascadeYamlErrors :: ModelT f -> R.LMLRoute -> [Text]
cascadeYamlErrors model r =
  flip mapMaybe (toList cascade) $ \rt -> do
    s <- modelLookupSData rt model
    leftToMaybe (s ^. sdataValue)
  where
    cascade = R.routeInits @'R.Yaml (R.withLmlRoute coerce r)
