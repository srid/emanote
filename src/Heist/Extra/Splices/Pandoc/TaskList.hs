{-# LANGUAGE RecordWildCards #-}

-- GFM Task List, https://github.github.com/gfm/#task-list-items-extension-
module Heist.Extra.Splices.Pandoc.TaskList
  ( parseTaskFromInlines,
  )
where

import Relude
import qualified Text.Pandoc.Builder as B

parseTaskFromInlines :: [B.Inline] -> Maybe (Bool, [B.Inline])
parseTaskFromInlines = \case
  B.Str "[" : B.Space : B.Str "]" : B.Space : taskInlines ->
    pure (False, taskInlines)
  B.Str "[x]" : B.Space : taskInlines ->
    pure (True, taskInlines)
  _ ->
    Nothing
