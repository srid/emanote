{-# LANGUAGE OverloadedStrings #-}

-- | Resource and tool surface exposed by the current Emanote MCP server.
module Emanote.MCP.Surface (
  resources,
  tools,
  readResource,
  callTool,
  serverInfoResourceUri,
  serverInfoToolName,
) where

import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Emanote.MCP.Info
import MCP.Protocol qualified as MP
import MCP.Types qualified as MT
import Relude

serverInfoResourceUri :: Text
serverInfoResourceUri = "emanote://server/info"

serverInfoToolName :: Text
serverInfoToolName = "emanote_get_server_info"

-- | Local resource registry entry pairing MCP metadata with its read handler.
data ResourceHandler = ResourceHandler
  { resourceKey :: Text
  , resourceDescriptor :: MT.Resource
  , readContents :: Maybe Text
  }

-- | Local tool registry entry pairing MCP metadata with its call handler.
data ToolHandler = ToolHandler
  { toolKey :: Text
  , toolDescriptor :: MT.Tool
  , runTool :: MP.CallToolParams -> Maybe MP.CallToolResult
  }

resources :: [MT.Resource]
resources = resourceDescriptor <$> resourceHandlers

tools :: [MT.Tool]
tools = toolDescriptor <$> toolHandlers

-- | Resolve a resource URI through the local resource registry.
readResource :: Text -> Either Text Text
readResource uri =
  maybeToRight ("Unknown MCP resource: " <> uri)
    $ asum (readContentsFor <$> resourceHandlers)
  where
    readContentsFor :: ResourceHandler -> Maybe Text
    readContentsFor handler =
      guard (uri == resourceKey handler) $> fromMaybe "" (readContents handler)

-- | Resolve a tool call through the local tool registry.
callTool :: MP.CallToolParams -> MP.CallToolResult
callTool params =
  fromMaybe unknownToolResult $ asum (runTool <$> toolHandlers <*> pure params)

resourceHandlers :: [ResourceHandler]
resourceHandlers =
  [ ResourceHandler
      { resourceKey = serverInfoResourceUri
      , resourceDescriptor =
          MT.Resource
            serverInfoResourceUri
            "server-info"
            (Just "Emanote MCP Server Info")
            (Just "Read basic runtime information about the Emanote MCP server")
            (Just "text/plain")
            Nothing
            Nothing
            Nothing
      , readContents = Just serverInfoText
      }
  ]

toolHandlers :: [ToolHandler]
toolHandlers =
  [ ToolHandler
      { toolKey = serverInfoToolName
      , toolDescriptor =
          MT.Tool
            serverInfoToolName
            (Just "Emanote MCP Server Info")
            (Just "Return basic runtime information about the Emanote MCP server")
            (MT.InputSchema "object" Nothing Nothing)
            Nothing
            (Just $ MT.ToolAnnotations (Just "Read-only") (Just True) (Just False) (Just True) (Just False))
            Nothing
      , runTool = runServerInfoTool
      }
  ]

runServerInfoTool :: MP.CallToolParams -> Maybe MP.CallToolResult
runServerInfoTool (MP.CallToolParams toolName toolArgs)
  | toolName == serverInfoToolName =
      Just
        $ MP.CallToolResult
          [MT.TextContentType $ MT.TextContent "text" "Emanote MCP server information." Nothing Nothing]
          (Just $ serverInfoPayload toolArgs)
          (Just False)
          Nothing
  | otherwise =
      Nothing

serverInfoText :: Text
serverInfoText =
  unlines
    [ "name: " <> implementationName
    , "title: " <> implementationTitle
    , "version: " <> implementationVersion
    , "protocolVersion: " <> protocolVersion
    , "resourceUris: " <> T.intercalate ", " [serverInfoResourceUri]
    , "toolNames: " <> T.intercalate ", " [serverInfoToolName]
    ]

-- | Structured server metadata returned by the info tool.
serverInfoPayload :: Maybe (Map.Map Text Aeson.Value) -> Aeson.Value
serverInfoPayload toolArgs =
  Aeson.object
    [ "server"
        Aeson..= Aeson.object
          [ "name" Aeson..= implementationName
          , "title" Aeson..= implementationTitle
          , "version" Aeson..= implementationVersion
          ]
    , "protocolVersion" Aeson..= protocolVersion
    , "resourceUris" Aeson..= [serverInfoResourceUri]
    , "toolNames" Aeson..= [serverInfoToolName]
    , "arguments" Aeson..= fromMaybe Map.empty toolArgs
    ]

unknownToolResult :: MP.CallToolResult
unknownToolResult =
  MP.CallToolResult
    [MT.TextContentType $ MT.TextContent "text" "Unknown tool" Nothing Nothing]
    Nothing
    (Just True)
    Nothing
