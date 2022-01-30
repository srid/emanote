-- GFM Task List, https://github.github.com/gfm/#task-list-items-extension-
module Heist.Extra.Splices.Pandoc.TaskList
  ( parseTaskFromInlines,
    queryTasks,
  )
where

import Relude
import Text.Pandoc.Builder qualified as B
import Text.Pandoc.Walk qualified as W

parseTaskFromInlines :: [B.Inline] -> Maybe (Bool, [B.Inline])
parseTaskFromInlines = \case
  B.Str "[" : B.Space : B.Str "]" : B.Space : taskInlines ->
    pure (False, taskInlines)
  B.Str "[x]" : B.Space : taskInlines ->
    pure (True, taskInlines)
  _ ->
    Nothing

queryTasks :: W.Walkable B.Block b => b -> [(Bool, [B.Inline])]
queryTasks =
  W.query $ \case
    B.Plain is ->
      maybeToList $ parseTaskFromInlines is
    B.Para is ->
      maybeToList $ parseTaskFromInlines is
    _ ->
      mempty
