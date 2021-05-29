module Emanote.Model.Query where

import Control.Lens.Operators
import Data.IxSet.Typed ((@=))
import qualified Data.IxSet.Typed as Ix
import qualified Data.Text as T
import Emanote.Model.Note
import Emanote.Model.Type
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

data Query
  = QueryByTag Text
  deriving (Eq, Show)

parseQuery :: Text -> Maybe Query
parseQuery = do
  rightToMaybe . parse queryParser "<pandoc:code:query>"
  where
    parse :: M.Parsec Void Text a -> String -> Text -> Either Text a
    parse p fn =
      first (toText . M.errorBundlePretty)
        . M.parse (p <* M.eof) fn

queryParser :: M.Parsec Void Text Query
queryParser = do
  void $ M.string "tag:#"
  QueryByTag . T.strip <$> M.takeRest

runQuery :: Model -> Query -> [Note]
runQuery model = \case
  QueryByTag tag ->
    Ix.toList $ (model ^. modelNotes) @= tag