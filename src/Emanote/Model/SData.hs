{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.Model.SData where

import Control.Lens.TH (makeLenses)
import qualified Data.Aeson as Aeson
import Data.Data (Data)
import Data.IxSet.Typed (Indexable (..), IxSet, ixGen, ixList)
import qualified Emanote.Route as R
import qualified Emanote.Route.Ext as Ext

-- | `S` for "structured". Refers to a per-route data file represented by Aeson
-- value.  Example: /foo/bar.yaml file
data SData = SData
  { _sdataValue :: Aeson.Value,
    _sdataRoute :: R.Route 'Ext.Yaml
  }
  deriving (Eq, Ord, Data, Show, Generic, Aeson.ToJSON)

type SDataIxs = '[R.Route 'Ext.Yaml]

type IxSData = IxSet SDataIxs SData

instance Indexable SDataIxs SData where
  indices =
    ixList
      (ixGen $ Proxy @(R.Route 'Ext.Yaml))

makeLenses ''SData
