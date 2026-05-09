{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | MCP request handlers.

Bridges "Emanote.MCP.Catalog" (notebook data) to "MCP.Server" wire
types. Handlers pull the current model via the 'IO' 'Model' reader
supplied at startup and translate 'Catalog.NotebookResource' /
'Catalog.ResourceBody' into MCP's 'Resource' / 'ReadResourceResult'.
-}
module Emanote.MCP.Handlers (
  handlers,
) where

import Emanote.MCP.Catalog (NotebookResource (..), ResourceBody (..))
import Emanote.MCP.Catalog qualified as Catalog
import Emanote.MCP.Uri (kindToUri, noteUriPrefix, noteUriTemplate, uriToKind)
import Emanote.Model (Model)
import MCP.Server (
  ListResourceTemplatesResult (..),
  ListResourcesResult (..),
  MCPHandlerState,
  MCPHandlerUser,
  ProcessResult (..),
  ReadResourceParams (..),
  ReadResourceResult (..),
  Resource (..),
  ResourceContents (..),
  ResourceTemplate (..),
  TextResourceContents (..),
  defaultProcessHandlers,
  listResourceTemplatesHandler,
  listResourcesHandler,
  readResourceHandler,
  withToolHandlers,
 )
import MCP.Server qualified as MCP
import Relude

type instance MCPHandlerState = ()

-- | Unused: 'MCP.Server.simpleHttpApp' bypasses the JWT pipeline that would consume this.
type instance MCPHandlerUser = ()

handlers :: IO Model -> MCP.ProcessHandlers
handlers readModel =
  withToolHandlers []
    $ defaultProcessHandlers
      { listResourcesHandler = Just $ \_ -> do
          model <- liftIO readModel
          pure
            $ ProcessSuccess
            $ ListResourcesResult
              { resources = toMcpResource <$> Catalog.listResources model
              , nextCursor = Nothing
              , MCP._meta = Nothing
              }
      , listResourceTemplatesHandler = Just $ \_ ->
          pure
            $ ProcessSuccess
            $ ListResourceTemplatesResult
              { resourceTemplates = [noteTemplate]
              , nextCursor = Nothing
              , MCP._meta = Nothing
              }
      , readResourceHandler = Just $ \ReadResourceParams {uri} ->
          case uriToKind uri of
            Nothing -> pure $ ProcessRPCError 404 $ "Resource not found: " <> uri
            Just kind -> do
              model <- liftIO readModel
              mBody <- liftIO $ Catalog.readResource model kind
              pure $ case mBody of
                Nothing -> ProcessRPCError 404 $ "Resource not found: " <> uri
                Just (ResourceBody mime body) ->
                  ProcessSuccess $ textResult uri mime body
      }

toMcpResource :: NotebookResource -> Resource
toMcpResource NotebookResource {resourceKind, resourceName, resourceTitle, resourceMime, resourceDescription} =
  Resource
    { MCP.uri = kindToUri resourceKind
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

noteTemplate :: ResourceTemplate
noteTemplate =
  ResourceTemplate
    { MCP.name = "Notebook note"
    , MCP.title = Just "Notebook note"
    , uriTemplate = noteUriTemplate
    , MCP.description = Just $ "Individual note by source path, e.g. " <> noteUriPrefix <> "guide/mcp.md"
    , MCP.mimeType = Just "text/markdown"
    , annotations = Nothing
    , MCP._meta = Nothing
    }
