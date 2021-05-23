{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.Model.Note where

import Control.Lens.Operators as Lens ((^.))
import Control.Lens.TH (makeLenses)
import qualified Data.Aeson as Aeson
import Data.Data (Data)
import Data.IxSet.Typed (Indexable (..), IxSet, ixFun, ixGen, ixList)
import qualified Emanote.PandocUtil as PandocUtil
import Emanote.Route (MarkdownRoute, Route)
import qualified Emanote.Route as R
import Emanote.Route.Ext
import qualified Emanote.Route.WikiLinkTarget as WL
import Text.Pandoc.Definition (Pandoc (..))

data Note = Note
  { _noteDoc :: Pandoc,
    _noteMeta :: Aeson.Value,
    _noteRoute :: Route ('LMLType 'Md)
  }
  deriving (Eq, Ord, Data, Show, Generic, Aeson.ToJSON)

-- | Set of WikiLinks that refer to a note.
newtype SelfRef = SelfRef {unSelfRef :: WL.WikiLinkTarget}
  deriving (Eq, Ord, Data, Show)

-- | Wiki-links that refer to this note.
noteSelfRefs :: Note -> [SelfRef]
noteSelfRefs =
  fmap SelfRef . toList . WL.allowedWikiLinkTargets . _noteRoute

type NoteIxs = '[MarkdownRoute, SelfRef]

type IxNote = IxSet NoteIxs Note

instance Indexable NoteIxs Note where
  indices =
    ixList
      (ixGen $ Proxy @MarkdownRoute)
      (ixFun noteSelfRefs)

makeLenses ''Note

noteTitle :: Note -> Text
noteTitle note =
  fromMaybe (R.routeFileBase $ note ^. noteRoute) $
    PandocUtil.getPandocTitle $ note ^. noteDoc
