{- | Notebook resource catalog consumed by "Emanote.MCP".

Answers two questions:

* /What/ is available? — 'listResources' returns catalog entries, one per
  static export ('MetadataJson', 'ContentMarkdown') and one per note.
* /How do I fetch one?/ — 'readResource' resolves a 'ResourceKind' to a
  'ResourceBody'.

The types here are MCP-independent (no URIs, no wire types), which
keeps the catalog easy to reuse if a second surface ever appears. The
module lives under "Emanote.MCP" because MCP is today's only consumer
and shares the catalog's change cadence.
-}
module Emanote.MCP.Catalog (
  ResourceKind (..),
  NotebookResource (..),
  ResourceBody (..),
  CatalogError (..),
  kindMime,
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

-- | MIME type of a resource, derived from its kind.
kindMime :: ResourceKind -> Text
kindMime = \case
  MetadataJson -> "application/json"
  ContentMarkdown -> "text/markdown"
  Note _ -> "text/markdown"

-- | Catalog entry. URI-free by design; consumers assign addressing.
data NotebookResource = NotebookResource
  { resourceKind :: ResourceKind
  , resourceName :: Text
  , resourceTitle :: Maybe Text
  , resourceDescription :: Maybe Text
  }

-- | Body payload for a resolved resource.
newtype ResourceBody = ResourceBody {resourceBodyText :: Text}

{- | Why 'readResource' couldn't return a body.

Distinguishes /the kind references nothing in the catalog/ from any
future IO-failure modes ('readNoteContent' surfaces a missing file as
'NotFound' today, since it can't tell that apart from a path with no
backing note in the model).
-}
data CatalogError = NotFound
  deriving stock (Show, Eq)

-- | Enumerate all resources the notebook currently exposes.
listResources :: Model -> [NotebookResource]
listResources model = staticResources <> noteResources model

staticResources :: [NotebookResource]
staticResources =
  [ NotebookResource
      { resourceKind = MetadataJson
      , resourceName = "Notebook metadata"
      , resourceTitle = Just "Notebook metadata (JSON)"
      , resourceDescription = Just "Notebook metadata as JSON: per-note titles, source paths, parent routes, and resolved links."
      }
  , NotebookResource
      { resourceKind = ContentMarkdown
      , resourceName = "Notebook content (single-file)"
      , resourceTitle = Just "Notebook content (single-file Markdown)"
      , resourceDescription = Just "All notes concatenated into a single Markdown document, separated by '===' delimiters."
      }
  ]

noteResources :: Model -> [NotebookResource]
noteResources model =
  [ NotebookResource
    { resourceKind = Note sourcePath
    , resourceName = toText sourcePath
    , resourceTitle = Just (Tit.toPlain (Note._noteTitle note))
    , resourceDescription = Nothing
    }
  | note <- toList (model ^. M.modelNotes)
  , let sourcePath = ExportJSON.lmlSourcePath (Note._noteRoute note)
  ]

{- | Resolve a 'ResourceKind' to its body.

Returns 'Left' 'NotFound' when a 'Note' kind references a path that
doesn't correspond to any known note, or when the note has no source
file (auto-generated notes).
-}
readResource :: Model -> ResourceKind -> IO (Either CatalogError ResourceBody)
readResource model = \case
  MetadataJson ->
    pure $ Right $ ResourceBody (decodeUtf8 (ExportJSON.renderJSONExport model))
  ContentMarkdown -> do
    body <- ExportContent.renderContentExport model
    pure $ Right $ ResourceBody body
  Note path ->
    case parseNoteRoute path >>= (`Note.lookupNotesByRoute` (model ^. M.modelNotes)) of
      Nothing -> pure $ Left NotFound
      Just note -> do
        mContent <- ExportContent.readNoteContent note
        pure $ case mContent of
          Nothing -> Left NotFound
          Just content ->
            let header = ExportContent.generateNoteHeader model note
             in Right $ ResourceBody (header <> content)

parseNoteRoute :: FilePath -> Maybe R.LMLRoute
parseNoteRoute fp =
  mkLMLRouteFromKnownFilePath Md fp <|> mkLMLRouteFromKnownFilePath Org fp
