{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.Route.Ext where

import Data.Aeson (ToJSON)
import Data.Data (Data)

data FileType = LMLType LML | Yaml | Html
  deriving (Generic, Eq, Show, Ord, Data, ToJSON)

-- | A lightweight markup language
--
-- https://en.wikipedia.org/wiki/Lightweight_markup_language
data LML = Md
  deriving (Generic, Eq, Show, Ord, Data, ToJSON)

class HasExt (ext :: FileType) where
  getExt :: String

instance HasExt ('LMLType 'Md) where
  getExt = ".md"

instance HasExt 'Yaml where
  getExt = ".yaml"

instance HasExt 'Html where
  getExt = ".html"
