{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.Model.SData where

import Control.Lens.TH (makeLenses)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Extra.Merge as AesonMerge
import Data.Data (Data)
import Data.IxSet.Typed (Indexable (..), IxSet, ixGen, ixList)
import qualified Data.List.NonEmpty as NE
import qualified Data.Yaml as Yaml
import qualified Emanote.Route as R

-- | `S` for "structured". Refers to a per-route data file represented by Aeson
-- value.  Example: /foo/bar.yaml file
data SData = SData
  { _sdataValue :: Aeson.Value,
    -- | Location of this data file
    _sdataRoute :: R.R 'R.Yaml
  }
  deriving (Eq, Ord, Data, Show, Generic, Aeson.ToJSON)

type SDataIxs = '[R.R 'R.Yaml]

type IxSData = IxSet SDataIxs SData

instance Indexable SDataIxs SData where
  indices =
    ixList
      (ixGen $ Proxy @(R.R 'R.Yaml))

makeLenses ''SData

parseSDataCascading :: R.R 'R.Yaml -> NonEmpty ByteString -> Either Text SData
parseSDataCascading r bs = do
  vals <- traverse (first (show @Text) . Yaml.decodeEither') bs
  let val = mergeAesons vals
  pure $ SData val r

-- | Later values override former.
mergeAesons :: NonEmpty Aeson.Value -> Aeson.Value
mergeAesons =
  NE.last . NE.scanl1 mergeAeson

mergeAeson :: Aeson.Value -> Aeson.Value -> Aeson.Value
mergeAeson = AesonMerge.lodashMerge
