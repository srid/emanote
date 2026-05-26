{- | Notebook resource catalog consumed by "Emanote.MCP".

Answers two questions in one declaration:

* /What/ is available? — the internal @resources@ list is the canonical list.
* /How do I fetch one?/ — each entry pairs a 'NotebookResource'
  description with a model-reading function.

'listResources' (for @resources\/list@) and 'readResource' (for
@resources\/read@) both derive from @resources@, so adding or removing
an entry updates advertising and serving in one place.

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
  listResources,
  readResource,
) where

import Emanote.Model (Model)
import Emanote.View.Export.JSON qualified as ExportJSON
import Relude

{- | Catalog entry. URI-bearing but MCP-wire-independent.

External clients hard-code 'resourceUri' — changing it is a breaking
protocol change.
-}
data NotebookResource = NotebookResource
  { resourceUri :: Text
  , resourceMime :: Text
  , resourceName :: Text
  , resourceTitle :: Maybe Text
  , resourceDescription :: Maybe Text
  }

-- | Body payload for a resolved resource.
newtype ResourceBody = ResourceBody {resourceBodyText :: Text}

{- | The full set of advertised resources, each paired with its renderer.

This is the single source of truth: 'listResources' projects out the
descriptions, 'readResource' looks up by URI to dispatch to the
renderer. Adding a new resource is one tuple here.

The renderer takes the live model so reads are not cached — every
@resources\/read@ re-runs.
-}
resources :: [(NotebookResource, Model -> ResourceBody)]
resources =
  [
    ( NotebookResource
        { resourceUri = "emanote://export/metadata"
        , resourceMime = "application/json"
        , resourceName = "Notebook metadata"
        , resourceTitle = Just "Notebook metadata (JSON)"
        , resourceDescription =
            Just
              "Notebook metadata as JSON: per-note titles, source paths, parent routes, and resolved links. Use this to discover note source paths, then read the files directly through your own filesystem tools."
        }
    , ResourceBody . decodeUtf8 . ExportJSON.renderJSONExport
    )
  ]

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
listResources = fst <$> resources

{- | Look up a resource by URI and render it against the current model.

Returns 'Nothing' when no advertised resource matches the URI.

__Complexity:__ /O(k)/ in the catalog size /k/ for the lookup, plus
the renderer's own cost (/O(N + R)/ for the metadata export).
-}
readResource :: Text -> Model -> Maybe (NotebookResource, ResourceBody)
readResource uri model =
  find (\(r, _) -> resourceUri r == uri) resources
    <&> \(r, render) -> (r, render model)
