{-# LANGUAGE OverloadedStrings #-}

module Emanote.MCP (
  startHttpServer,
  mcpResources,
  mcpTools,
  readPhase1Resource,
  callPhase1Tool,
) where

import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import MCP.Protocol qualified as MP
import MCP.Server
import MCP.Server.HTTP
import MCP.Types qualified as MT
import Relude

phase1ResourceUri :: Text
phase1ResourceUri = "emanote://phase-1/info"

phase1ToolName :: Text
phase1ToolName = "emanote_phase_1_status"

mcpResources :: [MT.Resource]
mcpResources =
  [ MT.Resource
      phase1ResourceUri
      "phase-1-info"
      (Just "Emanote MCP Phase 1")
      (Just "Minimal MCP runtime information exposed during phase 1")
      (Just "text/plain")
      Nothing
      Nothing
      Nothing
  ]

mcpTools :: [MT.Tool]
mcpTools =
  [ MT.Tool
      phase1ToolName
      (Just "Emanote MCP Phase 1 Status")
      (Just "Return the currently implemented MCP phase-1 surface")
      (MT.InputSchema "object" Nothing Nothing)
      Nothing
      (Just $ MT.ToolAnnotations (Just "Read-only") (Just True) (Just False) (Just True) (Just False))
      Nothing
  ]

readPhase1Resource :: Text -> Either Text Text
readPhase1Resource uri
  | uri == phase1ResourceUri =
      Right
        $ unlines
          [ "Emanote MCP phase 1 is enabled."
          , "This phase only wires the same-process HTTP runtime."
          , "Supported methods: initialize, resources/list, resources/read, tools/list, tools/call."
          , "Notebook-backed resources and query tools land in later phases."
          ]
  | otherwise =
      Left $ "Unknown MCP resource: " <> uri

callPhase1Tool :: MP.CallToolParams -> MP.CallToolResult
callPhase1Tool (MP.CallToolParams toolName toolArgs)
  | toolName == phase1ToolName =
      let payload =
            Aeson.object
              [ "phase" Aeson..= (1 :: Int)
              , "status" Aeson..= ("active" :: Text)
              , "resourceUris" Aeson..= [phase1ResourceUri]
              , "toolNames" Aeson..= [phase1ToolName]
              , "arguments" Aeson..= fromMaybe Map.empty toolArgs
              ]
       in MP.CallToolResult
            [MT.TextContentType $ MT.TextContent "text" "Emanote MCP phase 1 is active." Nothing Nothing]
            (Just payload)
            (Just False)
            Nothing
  | otherwise =
      MP.CallToolResult
        [MT.TextContentType $ MT.TextContent "text" "Unknown tool" Nothing Nothing]
        Nothing
        (Just True)
        Nothing

instance MCPServer MCPServerM where
  handleListResources _params =
    pure
      $ MP.ListResourcesResult mcpResources Nothing Nothing

  handleReadResource (MP.ReadResourceParams uri') =
    case readPhase1Resource uri' of
      Right contents ->
        pure
          $ MP.ReadResourceResult
            [MT.TextResource $ MT.TextResourceContents uri' contents (Just "text/plain") Nothing]
            Nothing
      Left err ->
        pure
          $ MP.ReadResourceResult
            [MT.TextResource $ MT.TextResourceContents uri' err (Just "text/plain") Nothing]
            Nothing

  handleListResourceTemplates _params =
    pure $ MP.ListResourceTemplatesResult [] Nothing Nothing

  handleListPrompts _params =
    pure $ MP.ListPromptsResult [] Nothing Nothing

  handleGetPrompt _params =
    pure $ MP.GetPromptResult (Just "Phase 1 does not expose prompts.") [] Nothing

  handleListTools _params =
    pure $ MP.ListToolsResult mcpTools Nothing Nothing

  handleCallTool params =
    pure $ callPhase1Tool params

  handleComplete _params =
    pure $ MP.CompleteResult (MP.CompletionResult [] (Just 0) (Just False)) Nothing

  handleSetLevel _params =
    liftIO pass

startHttpServer :: Int -> IO ()
startHttpServer port = do
  let implInfo =
        MT.Implementation "emanote-mcp" (Just "Emanote MCP") "1.6.0.0"
      capabilities =
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
          , httpServerInfo = implInfo
          , httpCapabilities = capabilities
          , httpEnableLogging = False
          , httpOAuthConfig = Nothing
          , httpJWK = Nothing
          , httpProtocolVersion = MT.mcpProtocolVersion
          }
  runServerHTTP config
