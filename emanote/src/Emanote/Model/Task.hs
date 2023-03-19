{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Emanote.Model.Task where

import Data.Aeson qualified as Aeson
import Data.IxSet.Typed (Indexable (..), IxSet, ixFun, ixList)
import Data.IxSet.Typed qualified as Ix
import Emanote.Model.Note (Note)
import Emanote.Model.Note qualified as N
import Emanote.Route qualified as R
import Heist.Extra.Splices.Pandoc.TaskList qualified as TaskList
import Optics.Operators ((^.))
import Optics.TH (makeLenses)
import Relude
import Text.Pandoc.Builder qualified as B

data Task = Task
  { _taskRoute :: R.LMLRoute
  , -- Index of this task within the containing note. Used to sort tasks by
    -- their original order of appearance in the Markdown file.
    _taskNum :: Word
  , _taskDescription :: [B.Inline]
  , _taskChecked :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON)

instance Ord Task where
  (<=) = (<=) `on` (_taskRoute &&& _taskNum)

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
  let taskListItems = TaskList.queryTasks $ note ^. N.noteDoc
   in Ix.fromList $
        zip [1 ..] taskListItems <&> \(idx, (checked, doc)) ->
          Task (note ^. N.noteRoute) idx doc checked

makeLenses ''Task
