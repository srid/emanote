{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Emabook.Model.SData where

import Control.Lens.TH (makeLenses)
import qualified Data.Aeson as Aeson
import Data.Data (Data)
import Data.IxSet.Typed (Indexable (..), IxSet, ixGen, ixList)
import qualified Emabook.Route as R

-- | `S` for "structured". Also to avoid conflict with builtin `Data`
data SData = SData
  { _sdataValue :: Aeson.Value,
    _sdataRoute :: R.Route R.Yaml
  }
  deriving (Eq, Ord, Data, Show, Generic, Aeson.ToJSON)

type SDataIxs = '[R.Route R.Yaml]

type IxSData = IxSet SDataIxs SData

instance Indexable SDataIxs SData where
  indices =
    ixList
      (ixGen $ Proxy @(R.Route R.Yaml))

makeLenses ''SData
