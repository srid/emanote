{-# LANGUAGE RecordWildCards #-}

-- | Export all notebook content to a single Markdown file
module Emanote.View.Export.Content (
  renderContentExport,
) where

import Data.IxSet.Typed qualified as Ix
import Data.Map.Strict qualified as Map
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
import Text.Pandoc (Pandoc, runPure)
import Text.Pandoc.Writers.Markdown (writeMarkdown)
import Text.Pandoc.Options (def)
import Data.Tree (Tree(..))
import Data.Tree qualified as Tree

-- | Export the notebook content to a single Markdown file
renderContentExport :: Model -> Maybe Text -> LByteString
renderContentExport model mDeployedSite = 
  let notes = getAllNotesInBFS model
      contentParts = map (renderNoteContent model mDeployedSite) notes
      fullContent = T.intercalate "\n\n---\n\n" contentParts
   in encodeUtf8 $ toLazy fullContent

-- | Get all notes in BFS order
getAllNotesInBFS :: Model -> [N.Note]
getAllNotesInBFS model = 
  let allNotes = Ix.toList $ M.modelNotes model
      -- Sort by route path to get a deterministic BFS-like ordering
      sortedNotes = sortOn (R.withLmlRoute R.encodeRoute . N._noteRoute) allNotes
   in sortedNotes

-- | Render a single note's content with metadata
renderNoteContent :: Model -> Maybe Text -> N.Note -> Text
renderNoteContent model mDeployedSite note =
  let route = note ^. N.noteRoute
      title = Tit.toPlain $ note ^. N.noteTitle
      noteUrl = case mDeployedSite of
        Just baseUrl -> baseUrl <> "/" <> SR.siteRouteUrl model (lmlSiteRoute (R.LMLView_Html, route))
        Nothing -> SR.siteRouteUrl model (lmlSiteRoute (R.LMLView_Html, route))
      pandocDoc = note ^. N.noteDoc
      markdownContent = case runPure $ writeMarkdown def pandocDoc of
        Left _ -> "<!-- Error converting note content -->"
        Right content -> content
      header = "# " <> title <> "\n\n" <>
               "**Source:** " <> noteUrl <> "\n\n"
   in header <> markdownContent