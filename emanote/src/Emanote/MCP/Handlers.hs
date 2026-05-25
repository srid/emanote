{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

{- | MCP request handlers.

Bridges "Emanote.MCP.Catalog" (notebook data) to "MCP.Server" wire
types. Handlers pull the current model via the 'IO' 'Model' reader
supplied at startup and translate 'Catalog.NotebookResource' /
'Catalog.ResourceBody' into MCP's 'Resource' / 'ReadResourceResult'.
-}
module Emanote.MCP.Handlers (
  handlers,
) where

import Emanote.MCP.Catalog (NotebookResource (..), ResourceBody (..), ResourceKind (..), kindMime)
import Emanote.MCP.Catalog qualified as Catalog
import Emanote.MCP.Uri (kindToUri, noteUriPrefix, noteUriTemplate, uriToKind)
import Emanote.Model (Model)
import MCP.Server (
  ListResourceTemplatesResult (..),
  ListResourcesResult (..),
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
              { resourceTemplates = mapMaybe templateFor allKindShapes
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
                Just (ResourceBody body) ->
                  ProcessSuccess $ textResult uri (kindMime kind) body
      }

toMcpResource :: NotebookResource -> Resource
toMcpResource NotebookResource {resourceKind, resourceName, resourceTitle, resourceDescription} =
  Resource
    { MCP.uri = kindToUri resourceKind
    , MCP.name = resourceName
    , MCP.title = resourceTitle
    , MCP.description = resourceDescription
    , MCP.mimeType = Just (kindMime resourceKind)
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

{- | One representative value per 'ResourceKind' constructor, used to drive
'templateFor' from 'listResourceTemplatesHandler'. The 'Note' path is
arbitrary — 'templateFor' only inspects the constructor.
-}
allKindShapes :: [ResourceKind]
allKindShapes = [MetadataJson, ContentMarkdown, Note ""]

{- | The MCP resource template for a kind, if it accepts a URI parameter.

Exhaustive on 'ResourceKind' so adding a new constructor forces a
decision about whether it deserves a template.
-}
templateFor :: ResourceKind -> Maybe ResourceTemplate
templateFor = \case
  MetadataJson -> Nothing
  ContentMarkdown -> Nothing
  Note _ ->
    Just
      $ ResourceTemplate
        { MCP.name = "Notebook note"
        , MCP.title = Just "Notebook note"
        , uriTemplate = noteUriTemplate
        , MCP.description = Just $ "Individual note by source path, e.g. " <> noteUriPrefix <> "guide/mcp.md"
        , MCP.mimeType = Just (kindMime (Note ""))
        , annotations = Nothing
        , MCP._meta = Nothing
        }
