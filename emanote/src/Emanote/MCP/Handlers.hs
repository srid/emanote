{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

{- | MCP request handlers.

Bridges "Emanote.MCP.Catalog" (notebook data) to "MCP.Server" wire
types. Handlers pull the current model via the 'IO' 'Model' reader
supplied at startup and translate 'Catalog.NotebookResource' /
'Catalog.ResourceBody' into MCP's 'Resource' / 'ReadResourceResult'.

__Per-request complexity__ (with /N/ = number of notes, /R/ = total
relations):

* @resources\/list@ — /O(1)/. Returns 'Catalog.listResources' verbatim.
* @resources\/templates\/list@ — /O(1)/. Currently empty.
* @resources\/read@ — /O(N + R)/ for the metadata export; other URIs
  return 400.
* @tools\/list@, @tools\/call@ — wired through "Emanote.MCP.Tools";
  see that module for per-tool complexity.

No caching: each call re-runs against the live model.
-}
module Emanote.MCP.Handlers (
  handlers,
) where

import Emanote.MCP.Catalog (NotebookResource (..), ResourceBody (..), metadataMime, metadataUri)
import Emanote.MCP.Catalog qualified as Catalog
import Emanote.MCP.Tools qualified as Tools
import Emanote.Model (Model)
import MCP.Server (
  ListResourceTemplatesResult (..),
  ListResourcesResult (..),
  ProcessResult (..),
  ReadResourceParams (..),
  ReadResourceResult (..),
  Resource (..),
  ResourceContents (..),
  TextResourceContents (..),
  defaultProcessHandlers,
  listResourceTemplatesHandler,
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
      , listResourceTemplatesHandler = Just $ \_ ->
          pure
            $ ProcessSuccess
            $ ListResourceTemplatesResult
              { resourceTemplates = []
              , nextCursor = Nothing
              , MCP._meta = Nothing
              }
      , readResourceHandler = Just $ \ReadResourceParams {uri} ->
          if uri == metadataUri
            then do
              model <- liftIO readModel
              let ResourceBody body = Catalog.readMetadata model
              pure $ ProcessSuccess $ textResult uri metadataMime body
            else pure $ ProcessRPCError 400 $ "Unrecognized resource URI: " <> uri
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
