{- | A primitive module to eventually pave way towards first-class "calendar"
(daily notes, etc.) support in Emanote; either built-in or as plugin.
-}
module Emanote.Model.Calendar where

import Emanote.Model.Calendar.Parser (parseRouteDay)
import Emanote.Model.Note qualified as N
import Emanote.Model.Title (Title)
import Emanote.Model.Type (Model, modelLookupTitle)
import Emanote.Route (LMLRoute)
import Relude

-- HACK: This is so that calendar backlinks are sorted properly.
backlinkSortKey :: Model -> LMLRoute -> Down Title
backlinkSortKey model =
  Down . flip modelLookupTitle model

-- HACK: Until we have a proper search support. This sorts query results for
-- timeline
noteSortKey :: N.Note -> (Down (Maybe Text), LMLRoute)
noteSortKey note =
  (Down $ N.lookupMeta @Text (one "date") note, N._noteRoute note)

isDailyNote :: LMLRoute -> Bool
isDailyNote =
  isJust . parseRouteDay
