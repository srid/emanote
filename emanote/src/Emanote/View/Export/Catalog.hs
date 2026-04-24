{- | Protocol-agnostic catalog of what an Emanote notebook exposes.

Sits between 'Emanote.Model' and any surface that wants to publish the
notebook (MCP today, possibly other transports later). Knows nothing
about MCP, URIs, or any wire format — the consumer translates
'ResourceKind' into its own addressing scheme.

The catalog answers two questions:

* /What/ is available? — 'listResources' returns catalog entries, one per
  static export ('MetadataJson', 'ContentMarkdown') and one per note.
* /How do I fetch one?/ — 'readResource' resolves a 'ResourceKind' to a
  'ResourceBody'.
-}
module Emanote.View.Export.Catalog (
  ResourceKind (..),
  NotebookResource (..),
  ResourceBody (..),
  listResources,
  readResource,
) where

import Emanote.Model (Model)
import Emanote.Model qualified as M
import Emanote.Model.Note qualified as Note
import Emanote.Model.Title qualified as Tit
import Emanote.Route qualified as R
import Emanote.Route.Ext (LML (Md, Org))
import Emanote.Route.ModelRoute (mkLMLRouteFromKnownFilePath)
import Emanote.View.Export.Content qualified as ExportContent
import Emanote.View.Export.JSON qualified as ExportJSON
import Optics.Operators ((^.))
import Relude

-- | A kind of resource the notebook exposes.
data ResourceKind
  = -- | Whole-notebook metadata as JSON.
    MetadataJson
  | -- | Whole-notebook concatenated Markdown.
    ContentMarkdown
  | -- | Individual note by source-relative path (e.g. @guide/mcp.md@).
    Note FilePath
  deriving stock (Show, Eq)

-- | Catalog entry. URI-free by design; consumers assign addressing.
data NotebookResource = NotebookResource
  { resourceKind :: ResourceKind
  , resourceName :: Text
  , resourceTitle :: Maybe Text
  , resourceMime :: Text
  , resourceDescription :: Maybe Text
  }

-- | Body payload for a resolved resource.
data ResourceBody = ResourceBody
  { resourceBodyMime :: Text
  , resourceBodyText :: Text
  }

-- | Enumerate all resources the notebook currently exposes.
listResources :: Model -> [NotebookResource]
listResources model = staticResources <> noteResources model

staticResources :: [NotebookResource]
staticResources =
  [ NotebookResource
      { resourceKind = MetadataJson
      , resourceName = "Notebook metadata"
      , resourceTitle = Just "Notebook metadata (JSON)"
      , resourceMime = "application/json"
      , resourceDescription = Just "Notebook metadata as JSON: per-note titles, source paths, parent routes, and resolved links."
      }
  , NotebookResource
      { resourceKind = ContentMarkdown
      , resourceName = "Notebook content (single-file)"
      , resourceTitle = Just "Notebook content (single-file Markdown)"
      , resourceMime = "text/markdown"
      , resourceDescription = Just "All notes concatenated into a single Markdown document, separated by '===' delimiters."
      }
  ]

noteResources :: Model -> [NotebookResource]
noteResources model =
  [ NotebookResource
    { resourceKind = Note sourcePath
    , resourceName = toText sourcePath
    , resourceTitle = Just (Tit.toPlain (Note._noteTitle note))
    , resourceMime = "text/markdown"
    , resourceDescription = Nothing
    }
  | note <- toList (model ^. M.modelNotes)
  , let sourcePath = ExportJSON.lmlSourcePath (Note._noteRoute note)
  ]

{- | Resolve a 'ResourceKind' to its body.

Returns 'Nothing' when a 'Note' kind references a path that doesn't
correspond to any known note, or when the note has no source file
(auto-generated notes).
-}
readResource :: Model -> ResourceKind -> IO (Maybe ResourceBody)
readResource model = \case
  MetadataJson ->
    pure $ Just $ ResourceBody "application/json" (decodeUtf8 (ExportJSON.renderJSONExport model))
  ContentMarkdown -> do
    body <- ExportContent.renderContentExport model
    pure $ Just $ ResourceBody "text/markdown" body
  Note path ->
    case parseNoteRoute path >>= (`Note.lookupNotesByRoute` (model ^. M.modelNotes)) of
      Nothing -> pure Nothing
      Just note -> do
        mContent <- ExportContent.readNoteContent note
        pure $ do
          content <- mContent
          let header = ExportContent.generateNoteHeader model note
          Just $ ResourceBody "text/markdown" (header <> content)

parseNoteRoute :: FilePath -> Maybe R.LMLRoute
parseNoteRoute fp =
  mkLMLRouteFromKnownFilePath Md fp <|> mkLMLRouteFromKnownFilePath Org fp
