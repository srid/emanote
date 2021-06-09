{-# LANGUAGE TypeApplications #-}

module Emanote.Model.Query where

import Control.Lens.Operators ((^.))
import Data.IxSet.Typed ((@=))
import qualified Data.IxSet.Typed as Ix
import qualified Data.Text as T
import Emanote.Model.Note (Note)
import qualified Emanote.Model.Note as N
import Emanote.Model.Type (Model, modelNotes)
import Emanote.Pandoc.Markdown.Syntax.HashTag (TagPattern)
import qualified Emanote.Pandoc.Markdown.Syntax.HashTag as HT
import qualified Emanote.Route as R
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Show as Show

data Query
  = QueryByTag TagPattern
  | QueryByPath FilePath
  deriving (Eq)

instance Show.Show Query where
  show = \case
    QueryByTag pat ->
      toString $ "Pages tagged #" <> HT.unTagPattern pat
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
  (M.string "tag:#" *> fmap (QueryByTag . HT.mkTagPattern . T.strip) M.takeRest)
    <|> (M.string "path:" *> fmap (QueryByPath . toString . T.strip) M.takeRest)

runQuery :: Model -> Query -> [Note]
runQuery model = \case
  QueryByTag pat ->
    -- TODO: We don't supporting filepattern-based matching (i.e., `foo/**`) yet.
    --
    -- Doing it requires some consideration, such as indexing it for effecient
    -- access. Or not?
    let asTag = HT.Tag $ toText $ HT.unTagPattern pat
     in Ix.toList $ (model ^. modelNotes) @= asTag
  QueryByPath path ->
    fromMaybe mempty $ do
      r <- R.mkRouteFromFilePath path
      pure $ Ix.toList $ (model ^. modelNotes) @= N.RAncestor r
