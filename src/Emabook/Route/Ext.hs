{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Emabook.Route.Ext where

import Data.Aeson (ToJSON)
import Data.Data (Data)

data Md
  deriving (Generic, Data, ToJSON)

data Yaml
  deriving (Generic, Data, ToJSON)

class Ext a where
  getExt :: Proxy a -> String

instance Ext Md where
  getExt Proxy = ".md"

instance Ext Yaml where
  getExt Proxy = ".yaml"
