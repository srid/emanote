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
import qualified Data.Aeson.Extra.Merge as AesonMerge
import qualified Data.IxSet.Typed as Ix
import qualified Data.List.NonEmpty as NE
import Emanote.Model (Model, modelData, modelDataDefault, modelLookup)
import Emanote.Model.Note
  ( noteMeta,
  )
import Emanote.Model.SData (sdataValue)
import qualified Emanote.Route as R
import Emanote.Route.Ext (FileType (LMLType), LML (Md))
import qualified Emanote.Route.Ext as Ext
import Relude.Extra.Map (StaticMap (lookup))

-- | Look up a specific key in the meta for a given route.
lookupMeta :: FromJSON a => a -> NonEmpty Text -> R.Route ('LMLType 'Md) -> Model -> a
lookupMeta x k r =
  lookupMetaFrom x k . getEffectiveRouteMeta r

lookupMetaFrom :: forall a. FromJSON a => a -> NonEmpty Text -> Aeson.Value -> a
lookupMetaFrom x (k :| ks) meta =
  fromMaybe x $ do
    Aeson.Object obj <- pure meta
    val <- lookup k obj
    case nonEmpty ks of
      Nothing -> resultToMaybe $ Aeson.fromJSON val
      Just ks' -> pure $ lookupMetaFrom x ks' val
  where
    resultToMaybe :: Aeson.Result b -> Maybe b
    resultToMaybe = \case
      Aeson.Error _ -> Nothing
      Aeson.Success b -> pure b

-- | Get the (final) metadata of a note at the given route, by merging it with
-- the defaults specified in parent routes all the way upto index.yaml.
getEffectiveRouteMeta :: R.Route ('LMLType 'Md) -> Model -> Aeson.Value
getEffectiveRouteMeta mr model = do
  let appDefault = model ^. modelDataDefault
  fromMaybe appDefault $ do
    let defaultFiles = R.routeInits @'Ext.Yaml (coerce mr)
    let defaults = flip mapMaybe (toList defaultFiles) $ \r -> do
          v <- fmap (^. sdataValue) . Ix.getOne . Ix.getEQ r $ model ^. modelData
          guard $ v /= Aeson.Null
          pure v
    let finalDefault = NE.last $ NE.scanl1 mergeAeson $ appDefault :| defaults
    pure $
      fromMaybe finalDefault $ do
        frontmatter <- (^. noteMeta) <$> modelLookup mr model
        guard $ frontmatter /= Aeson.Null -- To not trip up AesonMerge
        pure $ mergeAeson finalDefault frontmatter
  where

mergeAeson :: Aeson.Value -> Aeson.Value -> Aeson.Value
mergeAeson = AesonMerge.lodashMerge
