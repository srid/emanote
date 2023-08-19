{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}

module Emanote.Route.Ext where

import Data.Aeson (ToJSON)
import Data.Data (Data)
import Relude hiding (show)
import System.FilePath qualified as FP

data SourceExt = SourceExt
  deriving stock (Eq, Ord, Show, Read, Data, Generic)
  deriving anyclass (ToJSON)

data FileType a where
  LMLType :: LML -> FileType SourceExt
  Yaml :: FileType SourceExt
  Xml :: FileType ()
  HeistTpl :: FileType SourceExt
  -- | `AnyExt` has no *known* (at compile time) extension. It is used as a
  -- "catch all" type to capture files using an arbitrary.
  AnyExt :: FileType SourceExt
  Html :: FileType ()
  Folder :: FileType ()
  deriving stock (Typeable)

deriving stock instance Eq a => Eq (FileType a)

deriving stock instance Ord a => Ord (FileType a)

{- | A lightweight markup language

 https://en.wikipedia.org/wiki/Lightweight_markup_language

 This type exists simply because we may support more formats (eg: org-mode) in
 the future.
-}
data LML = Md | Org
  deriving stock (Generic, Eq, Ord, Typeable, Data, Enum, Bounded)
  deriving anyclass (ToJSON)

{- | The `HasExt` class's responsibility is to allow dealing with basepath sans
 extension (and vice-versa).
-}
class HasExt (ext :: FileType a) where
  fileType :: FileType a

  -- | Return the filepath with the known extension.
  withExt :: FilePath -> FilePath

  -- | Return the filepath without the known extension.
  withoutKnownExt :: FilePath -> Maybe FilePath

instance HasExt ('LMLType 'Md) where
  fileType = LMLType Md
  withExt = flip FP.addExtension ".md"
  withoutKnownExt = fpWithoutExt ".md"

instance HasExt ('LMLType 'Org) where
  fileType = LMLType Org
  withExt = flip FP.addExtension ".org"
  withoutKnownExt = fpWithoutExt ".org"

instance HasExt 'Yaml where
  fileType = Yaml
  withExt = flip FP.addExtension ".yaml"
  withoutKnownExt = fpWithoutExt ".yaml"

instance HasExt 'Html where
  fileType = Html
  withExt = flip FP.addExtension ".html"
  withoutKnownExt = fpWithoutExt ".html"

instance HasExt 'Xml where
  fileType = Xml
  withExt = flip FP.addExtension ".xml"
  withoutKnownExt = fpWithoutExt ".xml"

instance HasExt 'HeistTpl where
  fileType = HeistTpl
  withExt = flip FP.addExtension ".tpl"
  withoutKnownExt = fpWithoutExt ".tpl"

instance HasExt 'Folder where
  fileType = Folder
  withExt = id
  withoutKnownExt = pure

{- | The AnyExt instance ignores explicitly dealing with extensions, expecting
 the user to explicitly encode the extension in their value tpye.
-}
instance HasExt 'AnyExt where
  fileType = AnyExt
  withExt = id
  withoutKnownExt = pure

fpWithoutExt :: (Monad m, Alternative m) => String -> FilePath -> m FilePath
fpWithoutExt e fp = do
  let (base, ext) = FP.splitExtension fp
  guard $ ext == e
  pure base
