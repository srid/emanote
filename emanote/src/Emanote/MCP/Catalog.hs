{- | Notebook resource catalog consumed by "Emanote.MCP".

Answers two questions:

* /What/ is available? — 'listResources' returns catalog entries.
* /How do I fetch one?/ — 'readMetadata' produces the metadata payload.

The types here are MCP-independent (no MCP wire types), which keeps the
catalog easy to reuse if a second surface ever appears. The module
lives under "Emanote.MCP" because MCP is today's only consumer and
shares the catalog's change cadence.

Per-note content is /not/ a resource: clients discover note source
paths through the metadata export and read the underlying files
through their own filesystem tools. See 'Emanote.MCP' for the rationale.
-}
module Emanote.MCP.Catalog (
  NotebookResource (..),
  ResourceBody (..),
  metadataUri,
  metadataMime,
  metadataResource,
  listResources,
  readMetadata,
) where

import Emanote.Model (Model)
import Emanote.View.Export.JSON qualified as ExportJSON
import Relude

-- | URI of the notebook metadata export. External clients hard-code this.
metadataUri :: Text
metadataUri = "emanote://export/metadata"

-- | MIME type of the notebook metadata export.
metadataMime :: Text
metadataMime = "application/json"

-- | Catalog entry. URI-bearing but MCP-wire-independent.
data NotebookResource = NotebookResource
  { resourceUri :: Text
  , resourceMime :: Text
  , resourceName :: Text
  , resourceTitle :: Maybe Text
  , resourceDescription :: Maybe Text
  }

-- | Body payload for a resolved resource.
newtype ResourceBody = ResourceBody {resourceBodyText :: Text}

metadataResource :: NotebookResource
metadataResource =
  NotebookResource
    { resourceUri = metadataUri
    , resourceMime = metadataMime
    , resourceName = "Notebook metadata"
    , resourceTitle = Just "Notebook metadata (JSON)"
    , resourceDescription =
        Just
          "Notebook metadata as JSON: per-note titles, source paths, parent routes, and resolved links. Use this to discover note source paths, then read the files directly through your own filesystem tools."
    }

{- | Enumerate the resources advertised through MCP's @resources\/list@.

__Complexity:__ /O(1)/ — fixed one static entry, independent of
notebook size. Per-note resources are intentionally absent: enumerating
one entry per note makes @resources\/list@ scale linearly with notebook
size, which clients poll on every refresh and which inflates context
for clients that load the list eagerly. Clients discover note paths
from @emanote:\/\/export\/metadata@ (every note's @filePath@) and read
the underlying files via their own filesystem tools.
-}
listResources :: [NotebookResource]
listResources = [metadataResource]

{- | Render the notebook metadata as JSON.

__Complexity:__ /O(N + R)/ where /N/ = number of notes and /R/ = total
resolved relations. Iterates every note in
'Emanote.View.Export.JSON.renderJSONExport' and encodes the result.
-}
readMetadata :: Model -> ResourceBody
readMetadata = ResourceBody . decodeUtf8 . ExportJSON.renderJSONExport
