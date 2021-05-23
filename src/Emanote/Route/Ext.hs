{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.Route.Ext where

import Data.Aeson (ToJSON)
import Data.Data (Data)

data FileType = Md | Yaml
  deriving (Generic, Data, ToJSON)

class HasExt (ext :: FileType) where
  getExt :: String

instance HasExt 'Md where
  getExt = ".md"

instance HasExt 'Yaml where
  getExt = ".yaml"