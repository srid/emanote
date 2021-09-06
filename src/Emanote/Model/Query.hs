{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.Model.Query where

import Control.Lens.Operators ((^.))
import Data.IxSet.Typed ((@+), (@=))
import qualified Data.IxSet.Typed as Ix
import qualified Data.Text as T
import qualified Emanote.Model.Calendar as Calendar
import Emanote.Model.Note (Note)
import qualified Emanote.Model.Note as N
import Emanote.Model.Type (Model, modelNotes, modelTags)
import Emanote.Pandoc.Markdown.Syntax.HashTag (TagPattern)
import qualified Emanote.Pandoc.Markdown.Syntax.HashTag as HT
import qualified Emanote.Route as R
import System.FilePattern (FilePattern, (?==))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Show as Show

data Query
  = QueryByTag HT.Tag
  | QueryByTagPattern TagPattern
  | QueryByPath FilePath
  | QueryByPathPattern FilePattern
  deriving (Eq)

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

runQuery :: R.LMLRoute -> Model -> Query -> [Note]
runQuery currentRoute model =
  sortOn Calendar.noteSortKey . \case
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
    QueryByPathPattern (resolveDotInFilePattern -> pat) ->
      let notes = Ix.toList $ model ^. modelNotes
       in flip mapMaybe notes $ \note -> do
            guard $ pat ?== (R.encodeRoute . R.lmlRouteCase $ note ^. N.noteRoute)
            pure note
  where
    -- Resolve the ./ prefix which will for substituting "$PWD" in current
    -- note's route context.
    resolveDotInFilePattern (toText -> pat) =
      if "./" `T.isPrefixOf` pat
        then
          let folderR :: R.R 'R.Folder = coerce $ R.lmlRouteCase currentRoute
           in if folderR == R.indexRoute
                then -- If in "index.md", discard the ./
                  toString (T.drop 2 pat)
                else -- If in "$folder.md", discard the ./ and prepend with folder path prefix
                  R.encodeRoute folderR <> "/" <> toString (T.drop 2 pat)
        else toString pat
