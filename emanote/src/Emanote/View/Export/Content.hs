-- | Export all notebook content to a single Markdown file
module Emanote.View.Export.Content (
  renderContentExport,
)
where

import Data.IxSet.Typed qualified as Ix
import Data.Text qualified as T
import Emanote.Model (Model)
import Emanote.Model qualified as M
import Emanote.Model.Note qualified as N
import Emanote.Model.Title qualified as Tit
import Emanote.Route qualified as R
import Emanote.Route.SiteRoute qualified as SR
import Emanote.Route.SiteRoute.Class (lmlSiteRoute)
import Optics.Operators ((^.))
import Relude
import Text.Pandoc (def, runPure)
import Text.Pandoc.Writers.Markdown (writeMarkdown)

-- | Export the notebook content to a single Markdown file
renderContentExport :: Model -> Maybe Text -> LByteString
renderContentExport model mDeployedSite =
  let notes = getAllNotesInBFS model
   in if null notes
        then encodeUtf8 $ toLazy ("# Empty Notebook\n\nNo notes found in this notebook.\n" :: Text)
        else
          let contentParts = map (renderNoteContent model mDeployedSite) notes
              fullContent = T.intercalate "\n\n---\n\n" contentParts
              header =
                "# Notebook Content Export\n\n"
                  <> "This file contains all the content from the notebook, "
                  <> "exported for LLM consumption.\n\n"
                  <> "**Total Notes:** "
                  <> (show (length notes) :: Text)
                  <> "\n\n"
                  <> case mDeployedSite of
                    Just site -> "**Deployed Site:** " <> site <> "\n\n"
                    Nothing -> ""
           in encodeUtf8 $ toLazy $ header <> "---\n\n" <> fullContent

-- | Get all notes in BFS order
getAllNotesInBFS :: Model -> [N.Note]
getAllNotesInBFS model =
  let allNotes = Ix.toList $ model ^. M.modelNotes
      -- Filter out auto-generated placeholder notes and sort by hierarchy
      realNotes = filter (isJust . N._noteSource) allNotes
      -- Sort by route depth first (BFS-like), then by route path for deterministic ordering
      sortedNotes =
        sortOn
          ( \note ->
              let routePath = R.withLmlRoute R.encodeRoute (N._noteRoute note)
                  depth = length $ filter (== '/') routePath
               in (depth, routePath)
          )
          realNotes
   in sortedNotes

-- | Render a single note's content with metadata
renderNoteContent :: Model -> Maybe Text -> N.Note -> Text
renderNoteContent model mDeployedSite note =
  let route = note ^. N.noteRoute
      title = Tit.toPlain $ note ^. N.noteTitle
      routePath = R.withLmlRoute R.encodeRoute route
      noteUrl = case mDeployedSite of
        Just baseUrl ->
          let siteUrl = SR.siteRouteUrl model (lmlSiteRoute (R.LMLView_Html, route))
           in baseUrl <> (if "/" `T.isSuffixOf` baseUrl then "" else "/") <> siteUrl
        Nothing -> SR.siteRouteUrl model (lmlSiteRoute (R.LMLView_Html, route))
      pandocDoc = note ^. N.noteDoc
      markdownContent = case runPure $ writeMarkdown def pandocDoc of
        Left err -> "<!-- Error converting note content: " <> show err <> " -->"
        Right content -> content
      header =
        "# "
          <> title
          <> "\n\n"
          <> "**Source File:** `"
          <> toText routePath
          <> "`\n\n"
          <> "**URL:** "
          <> noteUrl
          <> "\n\n"
   in header <> markdownContent
