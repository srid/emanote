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
import qualified Emanote.Route as R
import qualified Heist.Extra.Splices.Pandoc.TaskList as TaskList
import Relude
import qualified Text.Pandoc.Builder as B

data Task = Task
  { _taskRoute :: R.LMLRoute,
    -- Index of this task within the containing note. Used to sort tasks by
    -- original user order.
    _taskNum :: Word,
    _taskDescription :: [B.Inline],
    _taskChecked :: Bool
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON)

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