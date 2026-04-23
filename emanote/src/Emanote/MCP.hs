{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | MCP (Model Context Protocol) server.

Runs alongside the Emanote live server in the same process, exposing a
read-only surface over HTTP. Phase 1 establishes the transport and the
minimal lifecycle (@initialize@, @resources/list@, @resources/read@,
@tools/list@, @tools/call@) with empty resource and tool inventories;
notebook-backed resources and query tools arrive in later phases.
-}
module Emanote.MCP (
  run,
) where

import Data.Version (showVersion)
import MCP.Server (
  Implementation (..),
  ListResourcesResult (..),
  LoggingLevel (..),
  MCPHandlerState,
  MCPHandlerUser,
  MCPServerState (..),
  ProcessResult (..),
  ReadResourceParams (..),
  ResourcesCapability (..),
  ServerCapabilities (..),
  ToolsCapability (..),
  defaultProcessHandlers,
  initMCPServerState,
  listResourcesHandler,
  readResourceHandler,
  simpleHttpApp,
  withToolHandlers,
 )
import MCP.Server qualified as MCP
import Network.Wai.Handler.Warp qualified as Warp
import Paths_emanote qualified
import Relude

-- | No per-session state is required for Phase 1.
type instance MCPHandlerState = ()

{- | No authenticated user type is required because we serve over
'simpleHttpApp', which bypasses the JWT pipeline entirely. The instance
exists only to satisfy the library's type-family constraint.
-}
type instance MCPHandlerUser = ()

{- | Start the MCP HTTP server on the given port.

This blocks. Intended to be run concurrently with the Emanote live
server via 'UnliftIO.Async.race_'.
-}
run :: Int -> IO ()
run port = do
  stateVar <- newMVar initialState
  putStrLn $ "MCP server listening on http://localhost:" <> show port <> "/mcp"
  Warp.run port (simpleHttpApp stateVar)

initialState :: MCPServerState
initialState =
  (initMCPServerState () Nothing Nothing capabilities implementation instructions handlers)
    { mcp_log_level = Just Warning
    }

implementation :: Implementation
implementation =
  Implementation
    { MCP.name = "emanote"
    , version = toText $ showVersion Paths_emanote.version
    , title = Just "Emanote MCP Server"
    }

instructions :: Maybe Text
instructions =
  Just
    "Emanote exposes its live notebook model over MCP. Phase 1 provides only the handshake; resources and tools arrive in later phases."

capabilities :: ServerCapabilities
capabilities =
  ServerCapabilities
    { logging = Nothing
    , prompts = Nothing
    , resources = Just ResourcesCapability {listChanged = Nothing, subscribe = Nothing}
    , tools = Just ToolsCapability {listChanged = Nothing}
    , completions = Nothing
    , experimental = Nothing
    }

handlers :: MCP.ProcessHandlers
handlers =
  withToolHandlers
    []
    defaultProcessHandlers
      { listResourcesHandler = Just $ \_ ->
          pure
            $ ProcessSuccess
            $ ListResourcesResult
              { resources = []
              , nextCursor = Nothing
              , MCP._meta = Nothing
              }
      , readResourceHandler = Just $ \ReadResourceParams {uri} ->
          pure $ ProcessRPCError 404 $ "Resource not found: " <> uri
      }
