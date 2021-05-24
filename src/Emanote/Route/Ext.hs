{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.Route.Ext where

import Data.Aeson (ToJSON)
import Data.Data (Data)
import qualified System.FilePath as FP

data FileType
  = LMLType LML
  | Yaml
  | Html
  | -- | `OtherExt` has no *known* (at compile time) extension. It is used as a
    -- "catch all" type to capture files using *all other* (unknown) extensions.
    OtherExt
  deriving (Generic, Eq, Show, Ord, Data, ToJSON)

-- | A lightweight markup language
--
-- https://en.wikipedia.org/wiki/Lightweight_markup_language
data LML = Md
  deriving (Generic, Eq, Show, Ord, Data, ToJSON)

class HasExt (ext :: FileType) where
  fileType :: FileType

  -- | Return the filepath with the known extension.
  withExt :: FilePath -> FilePath

  -- | Return the filepath without the known extension.
  withoutKnownExt :: FilePath -> Maybe FilePath

instance HasExt ('LMLType 'Md) where
  fileType = LMLType Md
  withExt = flip FP.addExtension ".md"
  withoutKnownExt = fpWithoutExt ".md"

instance HasExt 'Yaml where
  fileType = Yaml
  withExt = flip FP.addExtension ".yaml"
  withoutKnownExt = fpWithoutExt ".yaml"

instance HasExt 'Html where
  fileType = Html
  withExt = flip FP.addExtension ".html"
  withoutKnownExt = fpWithoutExt ".html"

-- | The OtherExt instance ignores explicit dealing with extensions, expecting
-- the user to explicitly encode the extenion in their value tpye.
instance HasExt 'OtherExt where
  fileType = OtherExt
  withExt = id
  withoutKnownExt = pure

fpWithoutExt :: (Monad m, Alternative m) => String -> FilePath -> m FilePath
fpWithoutExt e fp = do
  let (base, ext) = FP.splitExtension fp
  guard $ ext == e
  pure base