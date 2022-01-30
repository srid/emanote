{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Emanote.Model.StaticFile where

import Control.Lens.TH (makeLenses)
import Data.Aeson qualified as Aeson
import Data.IxSet.Typed (Indexable (..), IxSet, ixFun, ixList)
import Data.Time (UTCTime)
import Emanote.Pandoc.Markdown.Syntax.WikiLink qualified as WL
import Emanote.Route qualified as R
import Relude

data StaticFile = StaticFile
  { _staticFileRoute :: R.R @R.SourceExt 'R.AnyExt,
    _staticFilePath :: FilePath,
    -- | Indicates that this file was updated no latter than the given time.
    _staticFileTime :: UTCTime
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Aeson.ToJSON)

type StaticFileIxs = '[R.R 'R.AnyExt, WL.WikiLink]

type IxStaticFile = IxSet StaticFileIxs StaticFile

instance Indexable StaticFileIxs StaticFile where
  indices =
    ixList
      (ixFun $ one . _staticFileRoute)
      (ixFun $ toList . staticFileSelfRefs)

staticFileSelfRefs :: StaticFile -> NonEmpty WL.WikiLink
staticFileSelfRefs =
  fmap snd
    . WL.allowedWikiLinks
    . _staticFileRoute

makeLenses ''StaticFile
