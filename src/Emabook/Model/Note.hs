{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Emabook.Model.Note where

import Control.Lens.Operators as Lens ((^.))
import Control.Lens.TH (makeLenses)
import qualified Data.Aeson as Aeson
import Data.Data (Data)
import Data.IxSet.Typed (Indexable (..), IxSet, ixFun, ixGen, ixList)
import qualified Emabook.PandocUtil as PandocUtil
import Emabook.Route (MarkdownRoute)
import qualified Emabook.Route as R
import qualified Emabook.Route.WikiLinkTarget as WL
import Text.Pandoc.Definition (Pandoc (..))

data Note = Note
  { _noteDoc :: Pandoc,
    _noteMeta :: Aeson.Value,
    _noteRoute :: MarkdownRoute
  }
  deriving (Eq, Ord, Data, Show, Generic, Aeson.ToJSON)

-- | Set of WikiLinks that refer to a note.
newtype SelfRef = SelfRef {unSelfRef :: WL.WikiLinkTarget}
  deriving (Eq, Ord, Data, Show)

-- | Wiki-links that refer to this note.
noteSelfRefs :: Note -> [SelfRef]
noteSelfRefs = fmap SelfRef . toList . WL.allowedWikiLinkTargets . _noteRoute

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
