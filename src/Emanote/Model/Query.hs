{-# LANGUAGE TypeApplications #-}

module Emanote.Model.Query where

import Control.Lens.Operators ((^.))
import Data.IxSet.Typed ((@+), (@=))
import qualified Data.IxSet.Typed as Ix
import qualified Data.Text as T
import Emanote.Model.Note (Note)
import qualified Emanote.Model.Note as N
import Emanote.Model.Type (Model, modelNotes, modelTags)
import Emanote.Pandoc.Markdown.Syntax.HashTag (TagPattern)
import qualified Emanote.Pandoc.Markdown.Syntax.HashTag as HT
import qualified Emanote.Route as R
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Show as Show

data Query
  = QueryByTag HT.Tag
  | QueryByTagPattern TagPattern
  | QueryByPath FilePath
  deriving (Eq)

instance Show.Show Query where
  show = \case
    QueryByTag tag ->
      toString $ "Pages tagged #" <> HT.unTag tag
    QueryByTagPattern pat ->
      toString $ "Pages tagged by '" <> HT.unTagPattern pat <> "'"
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
  (M.string "tag:#" *> fmap (QueryByTag . HT.Tag . T.strip) M.takeRest)
    <|> (M.string "tag:" *> fmap (QueryByTagPattern . HT.mkTagPattern . T.strip) M.takeRest)
    <|> (M.string "path:" *> fmap (QueryByPath . toString . T.strip) M.takeRest)

runQuery :: Model -> Query -> [Note]
runQuery model = \case
  QueryByTag tag ->
    Ix.toList $ (model ^. modelNotes) @= tag
  QueryByTagPattern pat ->
    let allTags = fst <$> modelTags model
        matchingTags = filter (HT.tagMatch pat) allTags
     in Ix.toList $ (model ^. modelNotes) @+ matchingTags
  QueryByPath path ->
    fromMaybe mempty $ do
      r <- R.mkRouteFromFilePath path
      pure $ Ix.toList $ (model ^. modelNotes) @= N.RAncestor r
