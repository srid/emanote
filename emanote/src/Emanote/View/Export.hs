{-# LANGUAGE DeriveAnyClass #-}

-- | Export an Emanote notebook to external formats.
module Emanote.View.Export (
  ExportFormat (..),
  runExport,
  renderJSONExport,
  Link (..),
  modelRels,
) where

import Commonmark.Extensions.WikiLink qualified as WL
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
import Emanote.Source.Loc (locResolve)
import Optics.Operators ((^.))
import Relude

data ExportFormat
  = ExportFormat_Metadata
  | ExportFormat_Content Text FilePath

-- | Run the specified export format
runExport :: ExportFormat -> Model -> IO ()
runExport exportFormat model =
  case exportFormat of
    ExportFormat_Metadata -> do
      putLBSLn $ renderJSONExport model
    ExportFormat_Content baseUrl filename -> do
      content <- renderContentExport baseUrl model
      writeFileText filename content

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
  Map.fromListWith (<>) $
    M.modelNoteRels model
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

-- | Check if a note has a source file (i.e., it's an actual .md file from the notebook)
hasSourceFile :: Note.Note -> Bool
hasSourceFile note = isJust (Note._noteSource note)

-- | Export all notes to a single Markdown file, separated by delimiters
renderContentExport :: Text -> Model -> IO Text
renderContentExport baseUrl model = do
  let notes_ = model ^. M.modelNotes
      -- Only include notes that have a source file (actual .md files from the notebook)
      sourceNotes = filter hasSourceFile $ toList notes_
      noteList = sortOn (lmlSourcePath . Note._noteRoute) sourceNotes
  exportedNotes <- catMaybes <$> mapM (exportNote baseUrl model) noteList
  let llmPrompt =
        unlines
          [ "<!-- LLM PROMPT: This document contains all notes from an Emanote notebook."
          , "Each note is separated by '---' delimiters and includes metadata headers."
          , "- Source: The original file path in the notebook"
          , "- URL: The full URL where this note can be accessed"
          , "- Title: The note's title"
          , "- Wikilinks: All possible ways to reference this note using [[wikilink]] syntax"
          , ""
          , "When referencing notes, you can use any of the wikilinks provided."
          , "The base URL is: " <> baseUrl
          , "-->"
          , ""
          ]
  pure $ llmPrompt <> T.intercalate "\n\n---\n\n" exportedNotes

-- | Export a single note with metadata header
exportNote :: Text -> Model -> Note.Note -> IO (Maybe Text)
exportNote baseUrl model note = do
  case Note._noteSource note of
    Nothing -> pure Nothing -- Note has no source file (auto-generated)
    Just locAndFile -> do
      let actualFilePath = locResolve locAndFile
      markdownContent <- decodeUtf8 <$> readFileBS actualFilePath
      let route = Note._noteRoute note
          sourcePath = lmlSourcePath route
          noteUrl = baseUrl <> "/" <> toText (SR.siteRouteUrlStatic model $ lmlSiteRoute (R.LMLView_Html, route))
          noteTitle = Tit.toPlain $ Note._noteTitle note
          wikilinks = Note.noteSelfRefs note
          wikilinkTexts = toList $ fmap (toText . (show :: WL.WikiLink -> String)) wikilinks

          header =
            unlines
              [ "<!-- Source: " <> toText sourcePath <> " -->"
              , "<!-- URL: " <> noteUrl <> " -->"
              , "<!-- Title: " <> noteTitle <> " -->"
              , "<!-- Wikilinks: " <> T.intercalate ", " wikilinkTexts <> " -->"
              , ""
              ]

      pure $ Just $ header <> markdownContent
