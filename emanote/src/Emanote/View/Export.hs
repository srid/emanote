{-# LANGUAGE DeriveAnyClass #-}

-- | Export an Emanote notebook to external formats.
module Emanote.View.Export (
  renderJSONExport,
  renderContentExport,
  Link (..),
  modelRels,
) where

import Data.Aeson (ToJSON)
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Emanote.Model (Model)
import Emanote.Model qualified as M
import Emanote.Model.Link.Rel qualified as Rel
import Emanote.Model.Link.Resolve qualified as Resolve
import Emanote.Model.Note qualified as Note
import Emanote.Model.Title qualified as Tit
import Emanote.Route (LMLRoute)
import Emanote.Route qualified as R
import Emanote.Route.SiteRoute qualified as SR
import Emanote.Route.SiteRoute.Class (lmlSiteRoute)
import Optics.Operators ((^.))
import Relude
import Text.Pandoc.Writers.Markdown (writeMarkdown)
import Text.Pandoc.Options (def)
import Text.Pandoc (runPure)

-- | A JSON export of the notebook
data Export = Export
  { version :: Word
  -- ^ This JSON's schema version
  , files :: Map Text SourceFile
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

currentVersion :: Word
currentVersion = 1

-- | A source file in `Model`
data SourceFile = SourceFile
  { title :: Text
  , filePath :: Text
  , parentNote :: Maybe Text
  , url :: Text
  , meta :: Aeson.Value
  , links :: [Link]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

data Link = Link
  { unresolvedRelTarget :: Rel.UnresolvedRelTarget
  , resolvedRelTarget :: Rel.ResolvedRelTarget Text
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (ToJSON)

--- | Export the notebook's metadata (not content) to JSON format.
renderJSONExport :: Model -> LByteString
renderJSONExport model =
  let notes_ =
        M.modelNoteMetas model
          & Map.mapKeys lmlRouteKey
          & Map.map
            ( \(tit, r, meta_) ->
                let k = lmlRouteKey r
                 in SourceFile
                      (Tit.toPlain tit)
                      k
                      (toText . lmlSourcePath <$> M.parentLmlRoute model r)
                      (SR.siteRouteUrl model $ lmlSiteRoute (R.LMLView_Html, r))
                      meta_
                      (fromMaybe [] $ Map.lookup k rels)
            )
      rels = modelRels model & Map.mapKeys lmlRouteKey
      export = Export currentVersion notes_
   in Aeson.encode export

modelRels :: Model -> Map LMLRoute [Link]
modelRels model =
  Map.fromListWith (<>)
    $ M.modelNoteRels model
    <&> \rel ->
      let from_ = rel ^. Rel.relFrom
          to_ = rel ^. Rel.relTo
          toTarget =
            Resolve.resolveUnresolvedRelTarget model from_ to_
              <&> SR.siteRouteUrlStatic model
       in (from_, one $ Link to_ toTarget)

-- An unique key to represent this LMLRoute in the exported JSON
--
-- We use the source path consistently.
lmlRouteKey :: LMLRoute -> Text
lmlRouteKey =
  toText . R.withLmlRoute R.encodeRoute

-- Path of the LML note
lmlSourcePath :: LMLRoute -> FilePath
lmlSourcePath =
  R.withLmlRoute R.encodeRoute

-- | Export all notes to a single Markdown file, separated by delimiters
renderContentExport :: Text -> Model -> Text
renderContentExport baseUrl model =
  let notes_ = model ^. M.modelNotes
      noteList = sortOn (lmlSourcePath . Note._noteRoute) $ toList notes_
      exportedNotes = mapMaybe (exportNote baseUrl model) noteList
   in T.intercalate "\n\n---\n\n" exportedNotes

-- | Export a single note with metadata header
exportNote :: Text -> Model -> Note.Note -> Maybe Text
exportNote baseUrl model note = do
  let route = Note._noteRoute note
      sourcePath = lmlSourcePath route
      noteUrl = baseUrl <> "/" <> toText (SR.siteRouteUrlStatic model $ lmlSiteRoute (R.LMLView_Html, route))
      noteTitle = Tit.toPlain $ Note._noteTitle note
      doc = Note._noteDoc note
  
  -- Convert Pandoc document to Markdown
  markdownContent <- case runPure $ writeMarkdown def doc of
    Right md -> Just md
    Left _ -> Nothing
  
  let header = T.unlines
        [ "<!-- Source: " <> toText sourcePath <> " -->"
        , "<!-- URL: " <> noteUrl <> " -->"
        , "<!-- Title: " <> noteTitle <> " -->"
        , ""
        ]
  
  pure $ header <> markdownContent
