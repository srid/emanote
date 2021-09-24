{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.Route.Ext where

import Data.Aeson (ToJSON)
import Data.Data (Data)
import Relude hiding (show)
import qualified System.FilePath as FP

data FileType
  = LMLType LML
  | Yaml
  | HeistTpl
  | Html
  | Folder
  | -- | `AnyExt` has no *known* (at compile time) extension. It is used as a
    -- "catch all" type to capture files using an arbitrary.
    AnyExt
  deriving (Generic, Eq, Ord, Typeable, Data, ToJSON)

-- | A lightweight markup language
--
-- https://en.wikipedia.org/wiki/Lightweight_markup_language
data LML = Md
  deriving (Generic, Eq, Ord, Typeable, Data, ToJSON)

-- | The `HasExt` class's responsibility is to allow dealing with basepath sans
-- extension (and vice-versa).
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

instance HasExt 'HeistTpl where
  fileType = HeistTpl
  withExt = flip FP.addExtension ".tpl"
  withoutKnownExt = fpWithoutExt ".tpl"

instance HasExt 'Folder where
  fileType = Folder
  withExt = id
  withoutKnownExt = pure

-- | The AnyExt instance ignores explicitly dealing with extensions, expecting
-- the user to explicitly encode the extension in their value tpye.
instance HasExt 'AnyExt where
  fileType = AnyExt
  withExt = id
  withoutKnownExt = pure

fpWithoutExt :: (Monad m, Alternative m) => String -> FilePath -> m FilePath
fpWithoutExt e fp = do
  let (base, ext) = FP.splitExtension fp
  guard $ ext == e
  pure base
