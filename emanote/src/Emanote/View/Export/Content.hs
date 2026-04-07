{-# LANGUAGE QuasiQuotes #-}

-- | Content export functionality for Emanote notebooks.
module Emanote.View.Export.Content (
  renderContentExport,
  exportNote,
  generateNoteHeader,
  readNoteContent,
  noteDelimiter,
  llmPromptTemplate,
) where

import Commonmark.Extensions.WikiLink qualified as WL
import Data.Text qualified as T
import Emanote.Model (Model)
import Emanote.Model qualified as M
import Emanote.Model.Note qualified as Note
import Emanote.Model.Title qualified as Tit
import Emanote.Route qualified as R
import Emanote.Route.SiteRoute qualified as SR
import Emanote.Route.SiteRoute.Class (lmlSiteRoute)
import Emanote.Source.Loc (locResolve)
import Emanote.View.Export.JSON (getBaseUrlFromModel, lmlSourcePath)
import NeatInterpolation (text)
import Optics.Operators ((^.))
import Relude

-- | Delimiter used to separate notes in content export
noteDelimiter :: Text
noteDelimiter = "==="

-- | LLM prompt template for content export
llmPromptTemplate :: Text -> Text -> Text
llmPromptTemplate urlHelpText baseUrlText =
  [text|
  <!-- LLM PROMPT: This document contains all notes from an Emanote notebook.
  Each note is separated by '${noteDelimiter}' delimiters and includes metadata headers.
  - Source: The original file path in the notebook
  ${urlHelpText}
  - Title: The note's title
  - Wikilinks: All possible ways to reference this note using [[wikilink]] syntax

  When referencing notes, you can use any of the wikilinks provided.
  ${baseUrlText}
  -->
  |]
    <> "\n\n"

-- | Export all notes to a single Markdown file, separated by delimiters
renderContentExport :: Model -> IO Text
renderContentExport model = do
  let mBaseUrl = getBaseUrlFromModel model
  let notes_ = model ^. M.modelNotes
      noteList = sortOn (lmlSourcePath . Note._noteRoute) $ toList notes_
  exportedNotes <- catMaybes <$> mapM (exportNote model) noteList
  let urlHelpText = case mBaseUrl of
        Just _ -> "- URL: The full URL where this note can be accessed"
        Nothing -> "- URL: Not included (no base URL configured)"
      baseUrlText = case mBaseUrl of
        Just baseUrl -> "The base URL is: " <> baseUrl
        Nothing -> "No base URL configured (set page.siteUrl in notebook config)"
      llmPrompt = llmPromptTemplate urlHelpText baseUrlText
  pure $ llmPrompt <> T.intercalate ("\n\n" <> noteDelimiter <> "\n\n") exportedNotes

-- | Generate note header with metadata
generateNoteHeader :: Model -> Note.Note -> Text
generateNoteHeader model note =
  let route = Note._noteRoute note
      sourcePath = lmlSourcePath route
      noteTitle = Tit.toPlain $ Note._noteTitle note
      wikilinks = Note.noteSelfRefs note
      wikilinkTexts = toList $ fmap (toText . (show :: WL.WikiLink -> String)) wikilinks
      wikilinkList = T.intercalate ", " wikilinkTexts
      sourcePathText = toText sourcePath
      mBaseUrl = getBaseUrlFromModel model
   in case mBaseUrl of
        Just baseUrl ->
          let noteUrl = baseUrl <> "/" <> toText (SR.siteRouteUrlStatic model $ lmlSiteRoute (R.LMLView_Html, route))
           in [text|
             <!-- Source: ${sourcePathText} -->
             <!-- URL: ${noteUrl} -->
             <!-- Title: ${noteTitle} -->
             <!-- Wikilinks: ${wikilinkList} -->
             |]
                <> "\n\n"
        Nothing ->
          [text|
          <!-- Source: ${sourcePathText} -->
          <!-- Title: ${noteTitle} -->
          <!-- Wikilinks: ${wikilinkList} -->
          |]
            <> "\n\n"

-- | Read note content from file
readNoteContent :: Note.Note -> IO (Maybe Text)
readNoteContent note =
  case Note._noteSource note of
    Nothing -> pure Nothing -- Note has no source file (auto-generated)
    Just locAndFile -> do
      let actualFilePath = locResolve locAndFile
      markdownContent <- decodeUtf8 <$> readFileBS actualFilePath
      pure $ Just markdownContent

-- | Export a single note with metadata header
exportNote :: Model -> Note.Note -> IO (Maybe Text)
exportNote model note = do
  mContent <- readNoteContent note
  pure $ fmap (generateNoteHeader model note <>) mContent
