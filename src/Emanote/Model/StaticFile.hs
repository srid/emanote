{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.Model.StaticFile where

import Control.Lens.TH (makeLenses)
import qualified Data.Aeson as Aeson
import Data.IxSet.Typed (Indexable (..), IxSet, ixFun, ixList)
import Emanote.Model.SelfRef (SelfRef, routeSelfRefs)
import qualified Emanote.Route as R
import Emanote.Route.Ext (FileType (AnyExt))
import Emanote.Route.SomeRoute (liftSomeRoute)

data StaticFile = StaticFile
  { _staticFileRoute :: R.Route 'AnyExt,
    _staticFilePath :: FilePath
  }
  deriving (Eq, Ord, Show, Generic, Aeson.ToJSON)

type StaticFileIxs = '[R.Route 'AnyExt, SelfRef]

type IxStaticFile = IxSet StaticFileIxs StaticFile

instance Indexable StaticFileIxs StaticFile where
  indices =
    ixList
      (ixFun $ one . _staticFileRoute)
      (ixFun staticFileSelfRefs)

staticFileSelfRefs :: StaticFile -> [SelfRef]
staticFileSelfRefs =
  routeSelfRefs
    . liftSomeRoute
    . _staticFileRoute

makeLenses ''StaticFile
