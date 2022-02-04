{-# LANGUAGE DeriveAnyClass #-}

module Emanote.Model.Query.Type where

import Data.Aeson (ToJSON)
import Data.Text qualified as T
import Emanote.Pandoc.Markdown.Syntax.HashTag (TagPattern)
import Emanote.Pandoc.Markdown.Syntax.HashTag qualified as HT
import Relude
import System.FilePattern (FilePattern)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.Show qualified as Show

data Query
  = QueryByTag HT.Tag
  | QueryByTagPattern TagPattern
  | QueryByPath FilePath
  | QueryByPathPattern FilePattern
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (ToJSON)

instance Show.Show Query where
  show = \case
    QueryByTag tag ->
      toString $ "Pages tagged #" <> HT.unTag tag
    QueryByTagPattern pat ->
      toString $ "Pages tagged by '" <> HT.unTagPattern pat <> "'"
    QueryByPath p ->
      "Pages under path '/" <> p <> "'"
    QueryByPathPattern pat ->
      "Pages matching path '" <> pat <> "'"

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
    <|> (M.string "path:" *> fmap (fromUserPath . T.strip) M.takeRest)
  where
    fromUserPath s =
      if
          | "*" `T.isInfixOf` s ->
            QueryByPathPattern (toString s)
          | "/" `T.isPrefixOf` s ->
            QueryByPath (toString $ T.drop 1 s)
          | otherwise ->
            QueryByPathPattern (toString $ "**/" <> s <> "/**")
