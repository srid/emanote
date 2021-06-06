{-# LANGUAGE TypeApplications #-}

module Emanote.Model.Query where

import Control.Lens.Operators ((^.))
import Data.IxSet.Typed ((@=))
import qualified Data.IxSet.Typed as Ix
import qualified Data.Text as T
import Emanote.Model.Note (Note)
import qualified Emanote.Model.Note as N
import Emanote.Model.Type (Model, modelNotes)
import Emanote.Pandoc.Markdown.Syntax.HashTag (HashTag (..))
import qualified Emanote.Route as R
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Show as Show

data Query
  = QueryByTag HashTag
  | QueryByPath FilePath
  deriving (Eq)

instance Show.Show Query where
  show = \case
    QueryByTag (HashTag tag) ->
      toString $ "Pages tagged #" <> tag
    QueryByPath p ->
      "Pages under path '" <> p <> "'"

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
  (M.string "tag:#" *> fmap (QueryByTag . HashTag . T.strip) M.takeRest)
    <|> (M.string "path:" *> fmap (QueryByPath . toString . T.strip) M.takeRest)

runQuery :: Model -> Query -> [Note]
runQuery model = \case
  QueryByTag tag ->
    Ix.toList $ (model ^. modelNotes) @= tag
  QueryByPath path ->
    fromMaybe mempty $ do
      r <- R.mkRouteFromFilePath path
      pure $ Ix.toList $ (model ^. modelNotes) @= N.RAncestor r
