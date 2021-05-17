{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Emabook.Model.Meta where

import Control.Lens.Operators as Lens ((^.))
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Extra.Merge as AesonMerge
import Data.Default (Default (..))
import qualified Data.IxSet.Typed as Ix
import qualified Data.List.NonEmpty as NE
import Emabook.Model (Model, modelData, modelLookup)
import Emabook.Model.Note
  ( noteMeta,
  )
import Emabook.Model.SData (sdataValue)
import Emabook.Route (MarkdownRoute)
import qualified Emabook.Route as R
import qualified Emabook.Route.Ext as Ext
import Relude.Extra.Map (StaticMap (lookup))

-- | Look up a specific key in the meta for a given route.
lookupMeta :: (Default a, FromJSON a) => a -> Text -> MarkdownRoute -> Model -> a
lookupMeta x k r model =
  fromMaybe x $ do
    Aeson.Object obj <- pure $ getEffectiveRouteMeta r model
    resultToMaybe . Aeson.fromJSON =<< lookup k obj
  where
    resultToMaybe :: Aeson.Result b -> Maybe b
    resultToMaybe = \case
      Aeson.Error _ -> Nothing
      Aeson.Success b -> pure b

-- | Get the (final) metadata of a note at the given route, by merging it with
-- the defaults specified in parent routes all the way upto index.yaml.
getEffectiveRouteMeta :: MarkdownRoute -> Model -> Aeson.Value
getEffectiveRouteMeta mr model =
  -- NOTE: This should never return Aeson.Null as long there is an index.yaml
  -- TODO: Capture and warn of this invariant in user-friendly way.
  fromMaybe Aeson.Null $ do
    let defaultFiles = R.routeInits @Ext.Yaml (coerce mr)
    defaults <- nonEmpty $
      flip mapMaybe (toList defaultFiles) $ \r -> do
        v <- fmap (^. sdataValue) . Ix.getOne . Ix.getEQ r $ model ^. modelData
        guard $ v /= Aeson.Null
        pure v
    let finalDefault = NE.last $ NE.scanl1 mergeAeson defaults
    pure $
      fromMaybe finalDefault $ do
        frontmatter <- (^. noteMeta) <$> modelLookup mr model
        guard $ frontmatter /= Aeson.Null -- To not trip up AesonMerge
        pure $ mergeAeson finalDefault frontmatter
  where
    mergeAeson = AesonMerge.lodashMerge
