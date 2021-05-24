{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.Route.Ext where

import Data.Aeson (ToJSON)
import Data.Data (Data)
import System.FilePath (addExtension, splitExtension)

data FileType = LMLType LML | Yaml | Html | OtherExt
  deriving (Generic, Eq, Show, Ord, Data, ToJSON)

-- | A lightweight markup language
--
-- https://en.wikipedia.org/wiki/Lightweight_markup_language
data LML = Md
  deriving (Generic, Eq, Show, Ord, Data, ToJSON)

class HasExt (ext :: FileType) where
  fileType :: FileType
  addExt :: FilePath -> FilePath

  -- | Remove the extension from the filepath. Return Nothing if not of correct file type.
  removeExt :: FilePath -> Maybe FilePath

instance HasExt ('LMLType 'Md) where
  fileType = LMLType Md
  addExt = flip addExtension ".md"
  removeExt = fpWithoutExt ".md"

instance HasExt 'Yaml where
  fileType = Yaml
  addExt = flip addExtension ".yaml"
  removeExt = fpWithoutExt ".yaml"

instance HasExt 'Html where
  fileType = Html
  addExt = flip addExtension ".html"
  removeExt = fpWithoutExt ".html"

-- | The OtherExt instance ignores explicit dealing with extensions, expecting
-- the user to explicitly encode the extenion in their value tpye.
instance HasExt 'OtherExt where
  fileType = OtherExt
  addExt = id
  removeExt = pure

fpWithoutExt :: (Monad m, Alternative m) => String -> FilePath -> m FilePath
fpWithoutExt e fp = do
  let (base, ext) = splitExtension fp
  guard $ ext == e
  pure base