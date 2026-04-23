{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | MCP (Model Context Protocol) server.

Runs alongside the Emanote live server in the same process, exposing a
read-only surface over HTTP. Advertises the @resources@ and @tools@
capabilities; current handlers return empty inventories and a
not-found reply for unknown URIs.
-}
module Emanote.MCP (
  run,
) where

import Data.Version (showVersion)
import MCP.Server (
  Implementation (..),
  ListResourcesResult (..),
  MCPHandlerState,
  MCPHandlerUser,
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

type instance MCPHandlerState = ()

-- | Unused: 'simpleHttpApp' bypasses the JWT pipeline that would consume this.
type instance MCPHandlerUser = ()

{- | Start the MCP HTTP server on the given port.

This blocks. Intended to be run concurrently with the Emanote live
server via 'UnliftIO.Async.race_'.
-}
run :: Int -> IO ()
run port = do
  stateVar <- newMVar $ initMCPServerState () Nothing Nothing capabilities implementation instructions handlers
  Warp.run port (simpleHttpApp stateVar)

implementation :: Implementation
implementation =
  Implementation
    { MCP.name = "emanote"
    , version = toText $ showVersion Paths_emanote.version
    , title = Just "Emanote MCP Server"
    }

instructions :: Maybe Text
instructions = Just "Emanote notebook exposed over MCP."

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
