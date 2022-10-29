module Emanote.Model.Query where

import Data.IxSet.Typed ((@+), (@=))
import Data.IxSet.Typed qualified as Ix
import Data.Text qualified as T
import Emanote.Model.Calendar qualified as Calendar
import Emanote.Model.Note (Note)
import Emanote.Model.Note qualified as N
import Emanote.Model.Type (Model, modelNotes, modelTags)
import Emanote.Pandoc.Markdown.Syntax.HashTag (TagPattern)
import Emanote.Pandoc.Markdown.Syntax.HashTag qualified as HT
import Emanote.Route qualified as R
import Optics.Operators ((^.))
import Relude
import System.FilePattern (FilePattern, (?==))
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.Show qualified as Show

data Query
  = QueryByTag HT.Tag
  | QueryByTagPattern TagPattern
  | QueryByPath FilePath
  | QueryByPathPattern FilePattern
  deriving stock (Eq)

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
      maybeToMonoid $ do
        r <- R.mkRouteFromFilePath path
        pure $ Ix.toList $ (model ^. modelNotes) @= N.RAncestor r
    QueryByPathPattern (resolveDotInFilePattern -> pat) ->
      let notes = Ix.toList $ model ^. modelNotes
       in flip mapMaybe notes $ \note -> do
            guard $ pat ?== R.withLmlRoute R.encodeRoute (note ^. N.noteRoute)
            pure note
  where
    -- Resolve the ./ prefix which will for substituting "$PWD" in current
    -- note's route context.
    resolveDotInFilePattern (toText -> pat) =
      if "./" `T.isPrefixOf` pat
        then
          let folderR :: R.R 'R.Folder = R.withLmlRoute coerce currentRoute
           in if folderR == R.indexRoute
                then -- If in "index.md", discard the ./
                  toString (T.drop 2 pat)
                else -- If in "$folder.md", discard the ./ and prepend with folder path prefix
                  R.encodeRoute folderR <> "/" <> toString (T.drop 2 pat)
        else toString pat
