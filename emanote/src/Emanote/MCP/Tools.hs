{- | MCP wire adapter for the phase-3 query tools.

Translates between the @dpella/mcp@ 'ToolHandler' contract (JSON Schema in,
'CallToolResult' out) and the pure queries in "Emanote.MCP.ToolCatalog".

Three tools advertised:

* @find_notes@ — wraps 'ToolCatalog.findNotes'.
* @get_backlinks@ — wraps 'ToolCatalog.getBacklinks'.
* @resolve_wikilink@ — wraps 'ToolCatalog.resolveWikilink'.

Tools share the live 'Model' snapshot via the @'IO' 'Model'@ reader phase 2
plumbed; no shared 'IORef', no caching.
-}
module Emanote.MCP.Tools (
  tools,
) where

import Data.Aeson (ToJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Scientific (toBoundedInteger)
import Data.Text qualified as T
import Emanote.MCP.ToolCatalog qualified as TC
import Emanote.Model (Model)
import MCP.Server (
  CallToolResult,
  InputSchema (..),
  ProcessResult (..),
  ToolHandler,
  toolHandler,
  toolTextError,
  toolTextResult,
 )
import Relude

-- | All MCP tools, parameterized by the model reader.
tools :: IO Model -> [ToolHandler]
tools readModel =
  [ findNotesTool readModel
  , getBacklinksTool readModel
  , resolveWikilinkTool readModel
  ]

-- ---------------------------------------------------------------------------
-- find_notes
-- ---------------------------------------------------------------------------

findNotesTool :: IO Model -> ToolHandler
findNotesTool readModel =
  toolHandler
    "find_notes"
    (Just "Search the notebook for notes whose title or source path contains the query (case-insensitive). Returns up to `limit` matches.")
    InputSchema
      { schemaType = "object"
      , properties =
          Just
            $ Map.fromList
              [ ("query", stringProp "Substring to match against note title or source path.")
              , ("limit", intProp 1 100 "Maximum number of matches to return (default 20).")
              ]
      , required = Just ["query"]
      }
    $ \margs ->
      case readTextArg "query" margs of
        Left err -> pure $ toolError err
        Right q -> do
          let lim = fromMaybe 20 $ readIntArg "limit" margs
          model <- liftIO readModel
          pure $ toolJsonResult (Aeson.object ["matches" .= TC.findNotes q lim model])

-- ---------------------------------------------------------------------------
-- get_backlinks
-- ---------------------------------------------------------------------------

getBacklinksTool :: IO Model -> ToolHandler
getBacklinksTool readModel =
  toolHandler
    "get_backlinks"
    (Just "List notes that backlink to the note at the given source path.")
    InputSchema
      { schemaType = "object"
      , properties =
          Just
            $ Map.fromList
              [ ("path", stringProp "Note source path, e.g. guide/mcp.md.")
              ]
      , required = Just ["path"]
      }
    $ \margs ->
      case readTextArg "path" margs of
        Left err -> pure $ toolError err
        Right p -> do
          model <- liftIO readModel
          pure $ case TC.getBacklinks (toString p) model of
            Left err -> toolError err
            Right ms -> toolJsonResult (Aeson.object ["backlinks" .= ms])

-- ---------------------------------------------------------------------------
-- resolve_wikilink
-- ---------------------------------------------------------------------------

resolveWikilinkTool :: IO Model -> ToolHandler
resolveWikilinkTool readModel =
  toolHandler
    "resolve_wikilink"
    (Just "Resolve a wikilink (e.g. \"guide/mcp\") to a note or static file. Optionally relative to a source note for disambiguation; defaults to the notebook index.")
    InputSchema
      { schemaType = "object"
      , properties =
          Just
            $ Map.fromList
              [ ("wikilink", stringProp "Wikilink text without brackets, e.g. \"guide/mcp\".")
              , ("from", stringProp "Source note path (optional) used to disambiguate. Defaults to the notebook index.")
              ]
      , required = Just ["wikilink"]
      }
    $ \margs ->
      case readTextArg "wikilink" margs of
        Left err -> pure $ toolError err
        Right wl -> do
          let mFrom = toString <$> readTextArgMaybe "from" margs
          model <- liftIO readModel
          pure $ case TC.resolveWikilink wl mFrom model of
            Left err -> toolError err
            Right res -> toolJsonResult res

-- ---------------------------------------------------------------------------
-- Wire helpers
-- ---------------------------------------------------------------------------

toolJsonResult :: (ToJSON a) => a -> ProcessResult CallToolResult
toolJsonResult v = ProcessSuccess $ toolTextResult [decodeUtf8 (Aeson.encode v)]

toolError :: Text -> ProcessResult CallToolResult
toolError = ProcessSuccess . toolTextError

readTextArg :: Text -> Maybe (Map Text Aeson.Value) -> Either Text Text
readTextArg k margs =
  maybeToRight ("Argument \"" <> k <> "\" must be a non-empty string.") (readTextArgMaybe k margs)

readTextArgMaybe :: Text -> Maybe (Map Text Aeson.Value) -> Maybe Text
readTextArgMaybe k margs = do
  args <- margs
  Aeson.String t <- Map.lookup k args
  -- Normalise absent and empty-string to Nothing: some MCP clients omit
  -- optional fields, others send "". Callers see one uniform absent signal.
  guard (not (T.null t))
  pure t

-- | Returns 'Nothing' for non-integer JSON numbers and out-of-range values.
readIntArg :: Text -> Maybe (Map Text Aeson.Value) -> Maybe Int
readIntArg k margs = do
  args <- margs
  Aeson.Number n <- Map.lookup k args
  toBoundedInteger n

stringProp :: Text -> Aeson.Value
stringProp desc =
  Aeson.object ["type" .= ("string" :: Text), "description" .= desc]

intProp :: Int -> Int -> Text -> Aeson.Value
intProp lo hi desc =
  Aeson.object
    [ "type" .= ("integer" :: Text)
    , "minimum" .= lo
    , "maximum" .= hi
    , "description" .= desc
    ]
