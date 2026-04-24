{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | MCP (Model Context Protocol) server.

Runs alongside the Emanote live server in the same process, exposing the
notebook model as read-only MCP resources:

* @emanote:\/\/export\/metadata@ — JSON metadata for every note
* @emanote:\/\/export\/content@ — all notes concatenated as a single Markdown document
* @emanote:\/\/note\/{path}@ — an individual note by its source path

This module owns the MCP /protocol surface/ only — URI constants,
capability declarations, handshake text, and Resource/ResourceTemplate
wire types. The /notebook catalog/ (what's available, how to read it)
lives in "Emanote.MCP.Catalog". The two are bridged by
'uriToKind' \/ 'kindToUri' and 'toMcpResource'.

The live model is read via an 'IO Model' supplied by 'Emanote.run',
which builds it from 'Ema.Dynamic.currentValue' on the 'Dynamic'
produced by the site's 'siteInput'.
-}
module Emanote.MCP (
  run,
) where

import Data.Text qualified as T
import Data.Version (showVersion)
import Emanote.MCP.Catalog (NotebookResource (..), ResourceBody (..), ResourceKind (..))
import Emanote.MCP.Catalog qualified as Catalog
import Emanote.Model (Model)
import MCP.Server (
  Implementation (..),
  ListResourceTemplatesResult (..),
  ListResourcesResult (..),
  LoggingLevel (..),
  MCPHandlerState,
  MCPHandlerUser,
  MCPServerState (..),
  ProcessResult (..),
  ReadResourceParams (..),
  ReadResourceResult (..),
  Resource (..),
  ResourceContents (..),
  ResourceTemplate (..),
  ResourcesCapability (..),
  ServerCapabilities (..),
  TextResourceContents (..),
  ToolsCapability (..),
  defaultProcessHandlers,
  initMCPServerState,
  listResourceTemplatesHandler,
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

-- * URI schema (wire contract — external clients hard-code these)

metadataUri :: Text
metadataUri = "emanote://export/metadata"

contentUri :: Text
contentUri = "emanote://export/content"

noteUriPrefix :: Text
noteUriPrefix = "emanote://note/"

-- | RFC 6570 template for the per-note URI.
noteUriTemplate :: Text
noteUriTemplate = noteUriPrefix <> "{path}"

-- * Catalog ↔ URI translation

uriToKind :: Text -> Maybe ResourceKind
uriToKind uri
  | uri == metadataUri = Just MetadataJson
  | uri == contentUri = Just ContentMarkdown
  | Just path <- T.stripPrefix noteUriPrefix uri = Just (Note (toString path))
  | otherwise = Nothing

kindToUri :: ResourceKind -> Text
kindToUri = \case
  MetadataJson -> metadataUri
  ContentMarkdown -> contentUri
  Note path -> noteUriPrefix <> toText path

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

-- * Handlers

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
