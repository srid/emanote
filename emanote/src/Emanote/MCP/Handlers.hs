{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

{- | MCP request handlers.

Bridges "Emanote.MCP.Catalog" (notebook data) to "MCP.Server" wire
types. Handlers pull the current model via the 'IO' 'Model' reader
supplied at startup. Routing for both @resources\/list@ and
@resources\/read@ is derived from 'Catalog.resources', so adding a
catalog entry advertises and serves it without touching this module.

__Per-request complexity__ (with /N/ = number of notes, /R/ = total
relations):

* @resources\/list@ — /O(1)/. Returns 'Catalog.listResources' verbatim.
* @resources\/read@ — /O(k + N + R)/ where /k/ is the catalog size.
  Currently /k/ = 1, so effectively /O(N + R)/ for the metadata export.
* @tools\/list@, @tools\/call@ — wired through "Emanote.MCP.Tools";
  see that module for per-tool complexity.

@resources\/templates\/list@ is not advertised — Emanote has no
templated resources today, and 'defaultProcessHandlers' leaves the
slot unset so clients fall back to the @dpella\/mcp@ library default
(an empty list).

No caching: each call re-runs against the live model.
-}
module Emanote.MCP.Handlers (
  handlers,
) where

import Emanote.MCP.Catalog (NotebookResource (..), ResourceBody (..))
import Emanote.MCP.Catalog qualified as Catalog
import Emanote.MCP.Tools qualified as Tools
import Emanote.Model (Model)
import MCP.Server (
  ListResourcesResult (..),
  ProcessResult (..),
  ReadResourceParams (..),
  ReadResourceResult (..),
  Resource (..),
  ResourceContents (..),
  TextResourceContents (..),
  defaultProcessHandlers,
  listResourcesHandler,
  readResourceHandler,
  withToolHandlers,
 )
import MCP.Server qualified as MCP
import Relude

handlers :: IO Model -> MCP.ProcessHandlers
handlers readModel =
  withToolHandlers (Tools.tools readModel)
    $ defaultProcessHandlers
      { listResourcesHandler = Just $ \_ ->
          pure
            $ ProcessSuccess
            $ ListResourcesResult
              { resources = toMcpResource <$> Catalog.listResources
              , nextCursor = Nothing
              , MCP._meta = Nothing
              }
      , readResourceHandler = Just $ \ReadResourceParams {uri} -> do
          model <- liftIO readModel
          pure $ case Catalog.readResource uri model of
            Just (r, ResourceBody body) ->
              ProcessSuccess $ textResult uri (resourceMime r) body
            Nothing ->
              ProcessRPCError 400 $ "Unrecognized resource URI: " <> uri
      }

toMcpResource :: NotebookResource -> Resource
toMcpResource NotebookResource {resourceUri, resourceMime, resourceName, resourceTitle, resourceDescription} =
  Resource
    { MCP.uri = resourceUri
    , MCP.name = resourceName
    , MCP.title = resourceTitle
    , MCP.description = resourceDescription
    , MCP.mimeType = Just resourceMime
    , size = Nothing
    , annotations = Nothing
    , MCP._meta = Nothing
    }

textResult :: Text -> Text -> Text -> ReadResourceResult
textResult uri mime body =
  ReadResourceResult
    { contents =
        [ TextResource
            TextResourceContents
              { MCP.uri = uri
              , text = body
              , mimeType = Just mime
              , MCP._meta = Nothing
              }
        ]
    , MCP._meta = Nothing
    }
