module Emanote.Model.Calendar.Parser where

import Data.Time.Calendar (Day, fromGregorianValid)
import Emanote.Route (LMLRoute)
import Emanote.Route qualified as R
import Relude
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M

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
      -- Optional suffix (ignored)
      void
        $ optional
        $ do
          void $ M.oneOf ['-', '_', ' ']
          void M.takeRest
      maybe (fail "Not a date") pure
        $ fromGregorianValid year (fromInteger month) (fromInteger day)
