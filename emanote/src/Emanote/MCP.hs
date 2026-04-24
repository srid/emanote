{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | MCP (Model Context Protocol) server.

Runs alongside the Emanote live server in the same process, exposing the
notebook model as read-only MCP resources:

* @emanote:\/\/export\/metadata@ — JSON metadata for every note
* @emanote:\/\/export\/content@ — all notes concatenated as a single Markdown document
* @emanote:\/\/note\/{path}@ — an individual note by its source path

The live model is read via an 'IO Model' supplied by 'Emanote.run', which
builds it from 'Ema.Dynamic.currentValue' on the 'Dynamic' produced by
the site's 'siteInput'.
-}
module Emanote.MCP (
  run,
) where

import Data.Text qualified as T
import Data.Version (showVersion)
import Emanote.Model (Model)
import Emanote.Model qualified as M
import Emanote.Model.Note qualified as Note
import Emanote.Model.Title qualified as Tit
import Emanote.Route qualified as R
import Emanote.Route.Ext (LML (Md, Org))
import Emanote.Route.ModelRoute (mkLMLRouteFromKnownFilePath)
import Emanote.View.Export.Content qualified as ExportContent
import Emanote.View.Export.JSON qualified as ExportJSON
import MCP.Server (
  Implementation (..),
  ListResourceTemplatesResult (..),
  ListResourcesResult (..),
  LoggingLevel (..),
  MCPHandlerState,
  MCPHandlerUser,
  MCPServerState (..),
  MCPServerT,
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
import Optics.Operators ((^.))
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

The model reader is produced by 'Emanote.run' on top of
'Ema.Dynamic.currentValue'. It returns the initial model before any
update lands, and the latest pushed value thereafter — both reads are
non-blocking pointer loads.
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

metadataUri :: Text
metadataUri = "emanote://export/metadata"

contentUri :: Text
contentUri = "emanote://export/content"

noteUriPrefix :: Text
noteUriPrefix = "emanote://note/"

{- | RFC 6570 template for the per-note URI; referenced both in instructions
and in 'noteTemplate'.
-}
noteUriTemplate :: Text
noteUriTemplate = noteUriPrefix <> "{path}"

noteUri :: R.LMLRoute -> Text
noteUri route = noteUriPrefix <> toText (ExportJSON.lmlSourcePath route)

handlers :: IO Model -> MCP.ProcessHandlers
handlers readModel =
  withToolHandlers []
    $ defaultProcessHandlers
      { listResourcesHandler = Just $ \_ -> do
          model <- liftIO readModel
          pure
            $ ProcessSuccess
            $ ListResourcesResult
              { resources = staticResources <> noteResources model
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
      , readResourceHandler = Just $ \ReadResourceParams {uri} -> do
          model <- liftIO readModel
          readResource model uri
      }

readResource :: Model -> Text -> MCPServerT (ProcessResult ReadResourceResult)
readResource model uri
  | uri == metadataUri =
      pure
        $ ProcessSuccess
        $ textResult uri (Just "application/json")
        $ decodeUtf8 (ExportJSON.renderJSONExport model)
  | uri == contentUri = do
      body <- liftIO $ ExportContent.renderContentExport model
      pure $ ProcessSuccess $ textResult uri (Just "text/markdown") body
  | Just path <- T.stripPrefix noteUriPrefix uri =
      readNoteResource model uri (toString path)
  | otherwise = pure $ ProcessRPCError 404 $ "Resource not found: " <> uri

readNoteResource :: Model -> Text -> FilePath -> MCPServerT (ProcessResult ReadResourceResult)
readNoteResource model uri path =
  case parseNoteRoute path >>= (`Note.lookupNotesByRoute` (model ^. M.modelNotes)) of
    Nothing -> pure $ ProcessRPCError 404 $ "Note not found: " <> uri
    Just note -> do
      mContent <- liftIO $ ExportContent.readNoteContent note
      case mContent of
        Nothing -> pure $ ProcessRPCError 404 $ "Note has no source file: " <> uri
        Just content ->
          let header = ExportContent.generateNoteHeader model note
           in pure $ ProcessSuccess $ textResult uri (Just "text/markdown") (header <> content)

parseNoteRoute :: FilePath -> Maybe R.LMLRoute
parseNoteRoute fp =
  mkLMLRouteFromKnownFilePath Md fp <|> mkLMLRouteFromKnownFilePath Org fp

textResult :: Text -> Maybe Text -> Text -> ReadResourceResult
textResult uri mime body =
  ReadResourceResult
    { contents =
        [ TextResource
            TextResourceContents
              { MCP.uri = uri
              , text = body
              , mimeType = mime
              , MCP._meta = Nothing
              }
        ]
    , MCP._meta = Nothing
    }

staticResources :: [Resource]
staticResources =
  [ Resource
      { MCP.uri = metadataUri
      , MCP.name = "Notebook metadata"
      , MCP.title = Just "Notebook metadata (JSON)"
      , MCP.description = Just "Notebook metadata as JSON: per-note titles, source paths, parent routes, and resolved links."
      , MCP.mimeType = Just "application/json"
      , size = Nothing
      , annotations = Nothing
      , MCP._meta = Nothing
      }
  , Resource
      { MCP.uri = contentUri
      , MCP.name = "Notebook content (single-file)"
      , MCP.title = Just "Notebook content (single-file Markdown)"
      , MCP.description = Just "All notes concatenated into a single Markdown document, separated by '===' delimiters."
      , MCP.mimeType = Just "text/markdown"
      , size = Nothing
      , annotations = Nothing
      , MCP._meta = Nothing
      }
  ]

noteResources :: Model -> [Resource]
noteResources model =
  [ Resource
    { MCP.uri = noteUri (Note._noteRoute note)
    , MCP.name = toText (ExportJSON.lmlSourcePath (Note._noteRoute note))
    , MCP.title = Just $ Tit.toPlain (Note._noteTitle note)
    , MCP.description = Nothing
    , MCP.mimeType = Just "text/markdown"
    , size = Nothing
    , annotations = Nothing
    , MCP._meta = Nothing
    }
  | note <- toList (model ^. M.modelNotes)
  ]

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
