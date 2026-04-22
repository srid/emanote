{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Emanote.MCP.Server (
  startHttpServer,
) where

import Emanote.MCP.Info
import Emanote.MCP.Surface qualified as Surface
import MCP.Protocol qualified as MP
import MCP.Server
import MCP.Server.HTTP
import MCP.Types qualified as MT
import Relude

-- | dpella/mcp dispatch hook backed by Emanote's local MCP surface.
instance MCPServer MCPServerM where
  handleListResources _params =
    pure $ MP.ListResourcesResult Surface.resources Nothing Nothing

  handleReadResource (MP.ReadResourceParams uri') =
    pure
      $ MP.ReadResourceResult
        [ MT.TextResource
            $ MT.TextResourceContents
              uri'
              (either identity identity $ Surface.readResource uri')
              (Just "text/plain")
              Nothing
        ]
        Nothing

  handleListResourceTemplates _params =
    pure $ MP.ListResourceTemplatesResult [] Nothing Nothing

  handleListPrompts _params =
    pure $ MP.ListPromptsResult [] Nothing Nothing

  handleGetPrompt _params =
    pure $ MP.GetPromptResult (Just "This MCP surface does not expose prompts.") [] Nothing

  handleListTools _params =
    pure $ MP.ListToolsResult Surface.tools Nothing Nothing

  handleCallTool params =
    pure $ Surface.callTool params

  handleComplete _params =
    pure $ MP.CompleteResult (MP.CompletionResult [] (Just 0) (Just False)) Nothing

  handleSetLevel _params =
    liftIO pass

-- | Start the HTTP transport for the Emanote MCP server on the given port.
startHttpServer :: Int -> IO ()
startHttpServer port = do
  let capabilities =
        MT.ServerCapabilities
          Nothing
          (Just $ MT.PromptsCapability (Just False))
          (Just $ MT.ResourcesCapability (Just False) (Just False))
          (Just $ MT.ToolsCapability (Just False))
          Nothing
          Nothing
      config =
        HTTPServerConfig
          { httpPort = port
          , httpBaseUrl = "http://127.0.0.1:" <> show port
          , httpServerInfo = implementationInfo
          , httpCapabilities = capabilities
          , httpEnableLogging = False
          , httpOAuthConfig = Nothing
          , httpJWK = Nothing
          , httpProtocolVersion = protocolVersion
          }
  runServerHTTP config
