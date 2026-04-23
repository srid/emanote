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
import System.IO (hPutStrLn)

type instance MCPHandlerState = ()

-- | Unused: 'simpleHttpApp' bypasses the JWT pipeline that would consume this.
type instance MCPHandlerUser = ()

{- | Start the MCP HTTP server on the given port.

This blocks. Intended to be run concurrently with the Emanote live
server via 'UnliftIO.Async.race_'. Prints a single @listening@ line
to stderr once Warp has bound the socket. When @verbose@ is set, the
underlying @mcp@ library emits one line per request/response to
stdout.
-}
run :: Int -> Bool -> IO ()
run port verbose = do
  stateVar <-
    newMVar
      (initMCPServerState () Nothing Nothing capabilities implementation instructions handlers)
        { mcp_log_level = Just (if verbose then Debug else Warning)
        }
  let settings =
        Warp.defaultSettings
          & Warp.setPort port
          & Warp.setBeforeMainLoop
            (hPutStrLn stderr $ "[mcp] listening on http://localhost:" <> show port <> "/mcp")
  Warp.runSettings settings (simpleHttpApp stateVar)

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
