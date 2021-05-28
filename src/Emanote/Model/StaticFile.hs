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
import qualified Emanote.Route as R
import Emanote.Route.Ext (FileType (AnyExt))
import Emanote.Route.Linkable (liftLinkableRoute)
import qualified Emanote.WikiLink as WL

data StaticFile = StaticFile
  { _staticFileRoute :: R.Route 'AnyExt,
    _staticFilePath :: FilePath
  }
  deriving (Eq, Ord, Show, Generic, Aeson.ToJSON)

type StaticFileIxs = '[R.Route 'AnyExt, WL.WikiLink]

type IxStaticFile = IxSet StaticFileIxs StaticFile

instance Indexable StaticFileIxs StaticFile where
  indices =
    ixList
      (ixFun $ one . _staticFileRoute)
      (ixFun staticFileSelfRefs)

staticFileSelfRefs :: StaticFile -> [WL.WikiLink]
staticFileSelfRefs =
  WL.allowedWikiLinks
    . liftLinkableRoute
    . _staticFileRoute

makeLenses ''StaticFile
