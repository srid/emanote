{-# LANGUAGE DeriveAnyClass #-}

-- | Export an Emanote notebook to external formats.
module Emanote.View.Export (
  ExportFormat (..),
  renderExport,
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
import Emanote.Model.Meta (getEffectiveRouteMeta)
import Emanote.Model.Note qualified as Note
import Emanote.Model.SData (lookupAeson)
import Emanote.Model.Title qualified as Tit
import Emanote.Route (LMLRoute)
import Emanote.Route qualified as R
import Emanote.Route.SiteRoute qualified as SR
import Emanote.Route.SiteRoute.Class (lmlSiteRoute)
import Emanote.Route.SiteRoute.Type (ExportFormat (..))
import Emanote.Source.Loc (locResolve)
import Optics.Operators ((^.))
import Relude

-- | Render the specified export format to LByteString
renderExport :: ExportFormat -> Model -> IO LByteString
renderExport exportFormat model =
  case exportFormat of
    ExportFormat_Metadata -> do
      pure $ renderJSONExport model
    ExportFormat_Content -> do
      let mBaseUrl = getBaseUrlFromModel model
      content <- renderContentExport mBaseUrl model
      pure $ encodeUtf8 content

-- | Get base URL from model configuration (returns Nothing if not configured)
getBaseUrlFromModel :: Model -> Maybe Text
getBaseUrlFromModel model =
  let indexRoute = M.modelIndexRoute model
      feedMeta = getEffectiveRouteMeta indexRoute model
   in lookupAeson Nothing ("page" :| ["siteUrl"]) feedMeta

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

-- | Check if a note has a source file (i.e., it's an actual .md file from the notebook)
hasSourceFile :: Note.Note -> Bool
hasSourceFile note = isJust (Note._noteSource note)

-- | Export all notes to a single Markdown file, separated by delimiters
renderContentExport :: Maybe Text -> Model -> IO Text
renderContentExport mBaseUrl model = do
  let notes_ = model ^. M.modelNotes
      -- Only include notes that have a source file (actual .md files from the notebook)
      sourceNotes = filter hasSourceFile $ toList notes_
      noteList = sortOn (lmlSourcePath . Note._noteRoute) sourceNotes
  exportedNotes <- catMaybes <$> mapM (exportNote mBaseUrl model) noteList
  let baseUrlLine = case mBaseUrl of
        Just baseUrl -> ["The base URL is: " <> baseUrl]
        Nothing -> ["No base URL configured (set page.siteUrl in notebook config)"]
      llmPrompt =
        unlines
          $ [ "<!-- LLM PROMPT: This document contains all notes from an Emanote notebook."
            , "Each note is separated by '---' delimiters and includes metadata headers."
            , "- Source: The original file path in the notebook"
            ]
          <> ( case mBaseUrl of
                Just _ -> ["- URL: The full URL where this note can be accessed"]
                Nothing -> ["- URL: Not included (no base URL configured)"]
             )
          <> [ "- Title: The note's title"
             , "- Wikilinks: All possible ways to reference this note using [[wikilink]] syntax"
             , ""
             , "When referencing notes, you can use any of the wikilinks provided."
             ]
          <> baseUrlLine
          <> ["-->", ""]
  pure $ llmPrompt <> T.intercalate "\n\n---\n\n" exportedNotes

-- | Export a single note with metadata header
exportNote :: Maybe Text -> Model -> Note.Note -> IO (Maybe Text)
exportNote mBaseUrl model note = do
  case Note._noteSource note of
    Nothing -> pure Nothing -- Note has no source file (auto-generated)
    Just locAndFile -> do
      let actualFilePath = locResolve locAndFile
      markdownContent <- decodeUtf8 <$> readFileBS actualFilePath
      let route = Note._noteRoute note
          sourcePath = lmlSourcePath route
          noteTitle = Tit.toPlain $ Note._noteTitle note
          wikilinks = Note.noteSelfRefs note
          wikilinkTexts = toList $ fmap (toText . (show :: WL.WikiLink -> String)) wikilinks

          urlLine = case mBaseUrl of
            Just baseUrl ->
              let noteUrl = baseUrl <> "/" <> toText (SR.siteRouteUrlStatic model $ lmlSiteRoute (R.LMLView_Html, route))
               in ["<!-- URL: " <> noteUrl <> " -->"]
            Nothing -> []

          header =
            unlines
              $ [ "<!-- Source: " <> toText sourcePath <> " -->"
                ]
              <> urlLine
              <> [ "<!-- Title: " <> noteTitle <> " -->"
                 , "<!-- Wikilinks: " <> T.intercalate ", " wikilinkTexts <> " -->"
                 , ""
                 ]

      pure $ Just $ header <> markdownContent
