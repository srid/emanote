{-# LANGUAGE DuplicateRecordFields #-}

{- | MCP HTTP server setup.

Wires Warp to "Emanote.MCP.Handlers" and declares MCP server identity,
instructions, and capabilities. The caller-supplied 'IO' 'Model'
reader is passed straight through to the handlers.
-}
module Emanote.MCP.Server (
  run,
) where

import Data.Version (showVersion)
import Emanote.MCP.Handlers (handlers)
import Emanote.MCP.Uri (contentUri, metadataUri, noteUriPrefix, noteUriTemplate)
import Emanote.Model (Model)
import MCP.Server (
  Implementation (..),
  LoggingLevel (..),
  MCPServerState (..),
  ResourcesCapability (..),
  ServerCapabilities (..),
  ToolsCapability (..),
  initMCPServerState,
  simpleHttpApp,
 )
import MCP.Server qualified as MCP
import Network.Wai.Handler.Warp qualified as Warp
import Paths_emanote qualified
import Relude
import System.IO (hPutStrLn)

{- | Start the MCP HTTP server on the given port.

This blocks. Intended to be run concurrently with the Emanote live
server via 'UnliftIO.Async.race_'. Prints a single @listening@ line
to stderr once Warp has bound the socket. When @verbose@ is set, the
underlying @mcp@ library emits one line per request/response to
stdout.

The @'IO' 'Model'@ reader must be non-blocking — handlers call it
synchronously from the request path. 'Ema.Dynamic.currentValue'
satisfies this (it reads an 'IORef' seeded with the Dynamic's initial
value before the reader is returned), and is the intended source.
-}
run :: Int -> Bool -> IO Model -> IO ()
run port verbose readModel = do
  stateVar <-
    newMVar
      (initMCPServerState () Nothing Nothing capabilities implementation instructions (handlers readModel))
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
instructions =
  Just
    $ unlines
      [ "Emanote notebook exposed over MCP."
      , "Resources:"
      , "- " <> metadataUri <> " — JSON metadata for every note (titles, paths, parents, links)"
      , "- " <> contentUri <> " — all notes concatenated as a single Markdown document"
      , "- " <> noteUriTemplate <> " — individual note by source path (e.g. " <> noteUriPrefix <> "guide/mcp.md)"
      ]

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
