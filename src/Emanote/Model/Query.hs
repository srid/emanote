module Emanote.Model.Query where

import Control.Lens.Operators ((^.))
import Data.IxSet.Typed ((@+), (@=))
import Data.IxSet.Typed qualified as Ix
import Data.Text qualified as T
import Emanote.Model.Calendar qualified as Calendar
import Emanote.Model.Note (Note)
import Emanote.Model.Note qualified as N
import Emanote.Model.Query.Type (Query (..))
import Emanote.Model.Type (Model, modelNotes, modelTags)
import Emanote.Pandoc.Markdown.Syntax.HashTag qualified as HT
import Emanote.Route qualified as R
import Relude
import System.FilePattern ((?==))

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
