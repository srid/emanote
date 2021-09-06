{-# LANGUAGE TypeApplications #-}

-- | A primitive module to eventually pave way towards first-class "calendar"
-- (daily notes, etc.) support in Emanote; either built-in or as plugin.
module Emanote.Model.Calendar where

import qualified Emanote.Model.Note as N
import Emanote.Model.Title (Title)
import Emanote.Model.Type (Model, modelLookupTitle)
import Emanote.Route (LMLRoute)

-- HACK: This is so that calendar backlinks are sorted properly.
backlinkSortKey :: Model -> LMLRoute -> Down Title
backlinkSortKey model =
  Down . flip modelLookupTitle model

-- HACK: Until we have a proper search support. This sorts query results for
-- timeline
noteSortKey :: N.Note -> (Down (Maybe Text), Title)
noteSortKey note =
  (Down $ N.lookupMeta @Text (one "date") note, N._noteTitle note)
