{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.Model.Task where

import Control.Lens.Operators ((^.))
import Control.Lens.TH (makeLenses)
import qualified Data.Aeson as Aeson
import Data.IxSet.Typed (Indexable (..), IxSet, ixFun, ixList)
import qualified Data.IxSet.Typed as Ix
import Emanote.Model.Note (Note)
import qualified Emanote.Model.Note as N
import qualified Emanote.Pandoc.Markdown.Syntax.HashTag as HT
import qualified Emanote.Route as R
import Relude
import qualified Text.Pandoc.Builder as B

data Task = Task
  { _taskRoute :: R.LMLRoute,
    _taskDescription :: [B.Block],
    _taskTags :: Set HT.Tag
    -- TODO: due-date, etc.
  }
  deriving (Eq, Ord, Show, Generic, Aeson.ToJSON)

type TaskIxs =
  '[ -- Route to the note containing this task
     R.LMLRoute
   ]

type IxTask = IxSet TaskIxs Task

instance Indexable TaskIxs Task where
  indices =
    ixList
      (ixFun $ one . _taskRoute)

noteTasks :: Note -> IxTask
noteTasks note =
  -- TODO: implement it
  Ix.fromList [Task (note ^. N.noteRoute) [B.Plain $ one $ B.Str "Some task, testing"] mempty]

makeLenses ''Task