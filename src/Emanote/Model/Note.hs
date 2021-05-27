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
import Data.IxSet.Typed (Indexable (..), IxSet, ixFun, ixList)
import qualified Emanote.PandocUtil as PandocUtil
import qualified Emanote.Route as R
import Emanote.Route.SomeRoute
import qualified Emanote.Route.WikiLinkTarget as WL
import Text.Pandoc.Definition (Pandoc (..))

-- TODO: This type should be polymorphic over `LMLType` once we get to
-- supporting formats other than Markdown. Checkout out `SomeRoute`, but we need
-- that constrained to LML routes only.
data Note = Note
  { _noteDoc :: Pandoc,
    _noteMeta :: Aeson.Value,
    _noteRoute :: SomeLMLRoute
  }
  deriving (Eq, Ord, Show, Generic, Aeson.ToJSON)

-- | Set of WikiLinks that refer to a note.
newtype SelfRef = SelfRef {unSelfRef :: WL.WikiLinkTarget}
  deriving (Eq, Ord, Show)

-- | Wiki-links that refer to this note.
noteSelfRefs :: Note -> [SelfRef]
noteSelfRefs =
  fmap SelfRef . toList . WL.allowedWikiLinkTargets . _noteRoute

type NoteIxs = '[SomeLMLRoute, SelfRef]

type IxNote = IxSet NoteIxs Note

instance Indexable NoteIxs Note where
  indices =
    ixList
      (ixFun $ one . _noteRoute)
      (ixFun noteSelfRefs)

makeLenses ''Note

noteTitle :: Note -> Text
noteTitle note =
  fromMaybe (R.routeFileBase . someLMLRouteCase $ note ^. noteRoute) $
    PandocUtil.getPandocTitle $ note ^. noteDoc
