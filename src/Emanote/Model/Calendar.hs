-- | A primitive module to eventually pave way towards first-class "calendar"
-- (daily notes, etc.) support in Emanote; either built-in or as plugin.
module Emanote.Model.Calendar where

import Emanote.Model.Note qualified as N
import Emanote.Model.Title (Title)
import Emanote.Model.Type (Model, modelLookupTitle)
import Emanote.Route (LMLRoute)
import Emanote.Route qualified as R
import Relude
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M

-- HACK: This is so that calendar backlinks are sorted properly.
backlinkSortKey :: Model -> LMLRoute -> Down Title
backlinkSortKey model =
  Down . flip modelLookupTitle model

-- HACK: Until we have a proper search support. This sorts query results for
-- timeline
noteSortKey :: N.Note -> (Down (Maybe Text), Title)
noteSortKey note =
  (Down $ N.lookupMeta @Text (one "date") note, N._noteTitle note)

isDailyNote :: LMLRoute -> Bool
isDailyNote =
  isJust . M.parseMaybe parse . (R.routeBaseName . R.lmlRouteCase)
  where
    parse :: M.Parsec Void Text ()
    parse = do
      -- Year
      replicateM_ 4 M.digitChar
      void $ M.string "-"
      -- Month
      replicateM_ 2 M.digitChar
      void $ M.string "-"
      -- Day
      replicateM_ 2 M.digitChar
