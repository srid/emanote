-- | A primitive module to eventually pave way towards first-class "calendar"
-- (daily notes, etc.) support in Emanote; either built-in or as plugin.
module Emanote.Model.Calendar where

import Data.Time.Calendar (Day, fromGregorianValid)
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
noteSortKey :: N.Note -> (Down (Maybe Text), LMLRoute)
noteSortKey note =
  (Down $ N.lookupMeta @Text (one "date") note, N._noteRoute note)

isDailyNote :: LMLRoute -> Bool
isDailyNote =
  isJust . parseRouteDay

parseRouteDay :: LMLRoute -> Maybe Day
parseRouteDay =
  M.parseMaybe parse . R.withLmlRoute R.routeBaseName
  where
    parse :: M.Parsec Void Text Day
    parse = do
      let asInt = maybe (fail "Not an int") pure . readMaybe
      -- Year
      year <- asInt =<< replicateM 4 M.digitChar
      void $ M.string "-"
      -- Month
      month <- asInt =<< replicateM 2 M.digitChar
      void $ M.string "-"
      -- Day
      day <- asInt =<< replicateM 2 M.digitChar
      maybe (fail "Not a date") pure $
        fromGregorianValid year (fromInteger month) (fromInteger day)
