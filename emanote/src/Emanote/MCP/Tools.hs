{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

{- | MCP query tools (phase 3).

Three read-only tools exposed via MCP's @tools/list@ and @tools/call@:

* @find_notes@ — case-insensitive substring search over titles and source paths.
* @get_backlinks@ — wraps "Emanote.Model.Graph".'G.modelLookupBacklinks'.
* @resolve_wikilink@ — wraps "Emanote.Model.Link.Resolve".'Resolve.resolveWikiLinkMustExist'.

Tools read from the live 'Model' snapshot via the @'IO' 'Model'@ reader
phase 2 plumbed; no shared 'IORef', no caching.
-}
module Emanote.MCP.Tools (
  tools,

  -- * Pure helpers (exported for tests)
  NoteMatch (..),
  findNotes,
  getBacklinks,
  ResolveResult (..),
  resolveWikilink,
) where

import Commonmark.Extensions.WikiLink qualified as WL
import Data.Aeson (ToJSON (..), (.=))
import Data.Aeson qualified as Aeson
import Data.IxSet.Typed qualified as Ix
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Emanote.MCP.Uri (noteUriPrefix)
import Emanote.Model (Model)
import Emanote.Model qualified as M
import Emanote.Model.Graph qualified as G
import Emanote.Model.Link.Rel qualified as Rel
import Emanote.Model.Link.Resolve qualified as Resolve
import Emanote.Model.Note qualified as N
import Emanote.Model.StaticFile qualified as SF
import Emanote.Model.Title qualified as Tit
import Emanote.Route qualified as R
import MCP.Server (
  CallToolResult,
  InputSchema (..),
  ProcessResult (..),
  ToolHandler,
  toolHandler,
  toolTextError,
  toolTextResult,
 )
import Network.URI.Slug qualified as Slug
import Optics.Operators ((^.))
import Relude

-- ---------------------------------------------------------------------------
-- Public entry point
-- ---------------------------------------------------------------------------

-- | All MCP tools, parameterized by the model reader.
tools :: IO Model -> [ToolHandler]
tools readModel =
  [ findNotesTool readModel
  , getBacklinksTool readModel
  , resolveWikilinkTool readModel
  ]

-- ---------------------------------------------------------------------------
-- Common types
-- ---------------------------------------------------------------------------

-- | A single note hit returned by 'findNotes' and 'getBacklinks'.
data NoteMatch = NoteMatch
  { path :: Text
  , title :: Text
  }
  deriving stock (Eq, Show, Generic)

{- | The @uri@ field is derived from @path@ so there is no way for the two to
diverge: drift in 'noteUriPrefix' propagates to every consumer through
one place.
-}
instance ToJSON NoteMatch where
  toJSON NoteMatch {path, title} =
    Aeson.object
      [ "path" .= path
      , "title" .= title
      , "uri" .= (noteUriPrefix <> path)
      ]

noteMatchOf :: N.Note -> NoteMatch
noteMatchOf note =
  NoteMatch
    { path = toText $ R.lmlSourcePath (note ^. N.noteRoute)
    , title = Tit.toPlain (note ^. N.noteTitle)
    }

{- | Build a 'NoteMatch' from a route. Falls back to a route-derived title
when the note can't be looked up — used by callers that hold a route but
not the 'N.Note' (e.g. backlink sources).
-}
noteMatchOfRoute :: Model -> R.LMLRoute -> NoteMatch
noteMatchOfRoute model r =
  maybe fallback noteMatchOf (M.modelLookupNoteByRoute' r model)
  where
    fallback =
      NoteMatch
        { path = toText $ R.lmlSourcePath r
        , title = Tit.toPlain (Tit.fromRoute r)
        }

-- ---------------------------------------------------------------------------
-- find_notes
-- ---------------------------------------------------------------------------

{- | Substring search (case-insensitive) over note titles and source paths.

Returns up to @limit@ matches in 'IxSet' iteration order; this is stable
under a given model snapshot but not lexicographically sorted. Callers that
want ordered output should sort downstream.
-}
findNotes :: Text -> Int -> Model -> [NoteMatch]
findNotes query lim model =
  let q = T.toLower query
      hit note =
        let m = noteMatchOf note
         in if q `T.isInfixOf` T.toLower (title m) || q `T.isInfixOf` T.toLower (path m)
              then Just m
              else Nothing
   in take (max 0 lim) $ mapMaybe hit $ Ix.toList (model ^. M.modelNotes)

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
          pure $ toolJsonResult (Aeson.object ["matches" .= findNotes q lim model])

-- ---------------------------------------------------------------------------
-- get_backlinks
-- ---------------------------------------------------------------------------

{- | Backlinks for the note at the given source path.

Returns 'Left' if @path@ isn't a recognised LML source path
(@guide/mcp.md@, @daily/2024-01-01.org@, …). An empty list is a valid
'Right' result and means the note exists but no other note links to it.
-}
getBacklinks :: FilePath -> Model -> Either Text [NoteMatch]
getBacklinks fp model =
  case R.mkLMLRouteFromMdOrOrgFilePath fp of
    Nothing -> Left $ "Not a recognised note path: " <> toText fp
    Just r ->
      Right $ noteMatchOfRoute model . fst <$> G.modelLookupBacklinks r model

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
          pure $ case getBacklinks (toString p) model of
            Left err -> toolError err
            Right ms -> toolJsonResult (Aeson.object ["backlinks" .= ms])

-- ---------------------------------------------------------------------------
-- resolve_wikilink
-- ---------------------------------------------------------------------------

-- | Outcome of resolving a wikilink, mirroring 'Rel.ResolvedRelTarget'.
data ResolveResult
  = ResolvedNote NoteMatch
  | ResolvedStatic Text
  | UnresolvedMissing
  | UnresolvedAmbiguous [Either NoteMatch Text]
  deriving stock (Eq, Show, Generic)

instance ToJSON ResolveResult where
  toJSON = \case
    ResolvedNote nm ->
      Aeson.object ["result" .= ("found" :: Text), "kind" .= ("note" :: Text), "note" .= nm]
    ResolvedStatic p ->
      Aeson.object ["result" .= ("found" :: Text), "kind" .= ("static" :: Text), "path" .= p]
    UnresolvedMissing ->
      Aeson.object ["result" .= ("missing" :: Text)]
    UnresolvedAmbiguous cs ->
      Aeson.object
        [ "result" .= ("ambiguous" :: Text)
        , "candidates" .= (candidateValue <$> cs)
        ]
    where
      candidateValue = \case
        Left nm -> Aeson.object ["kind" .= ("note" :: Text), "note" .= nm]
        Right p -> Aeson.object ["kind" .= ("static" :: Text), "path" .= p]

{- | Resolve a wikilink string (without brackets), optionally relative to a
source note for ambiguity disambiguation. Defaults the @from@ context to
the notebook index when unspecified.
-}
resolveWikilink :: Text -> Maybe FilePath -> Model -> Either Text ResolveResult
resolveWikilink wlText mFromPath model = do
  wl <- maybeToRight ("Not a valid wikilink: " <> wlText) (parseWikiLinkText wlText)
  fromR <- case mFromPath of
    Nothing -> Right (M.modelIndexRoute model)
    Just p -> maybeToRight ("Not a recognised note path: " <> toText p) (R.mkLMLRouteFromMdOrOrgFilePath p)
  Right $ case Resolve.resolveWikiLinkMustExist model fromR wl of
    Rel.RRTFound (Left (_, note)) -> ResolvedNote (noteMatchOf note)
    Rel.RRTFound (Right sf) -> ResolvedStatic (staticFilePath sf)
    Rel.RRTMissing -> UnresolvedMissing
    Rel.RRTAmbiguous cs -> UnresolvedAmbiguous $ toList $ candidate <$> cs
  where
    candidate = \case
      Left (_, note) -> Left (noteMatchOf note)
      Right sf -> Right (staticFilePath sf)
    staticFilePath sf = toText $ R.encodeRoute (sf ^. SF.staticFileRoute)

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
          pure $ case resolveWikilink wl mFrom model of
            Left err -> toolError err
            Right res -> toolJsonResult res

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Parse a slash-separated wikilink target (e.g. "foo/bar") into a 'WL.WikiLink'.
parseWikiLinkText :: Text -> Maybe WL.WikiLink
parseWikiLinkText s
  | T.null s = Nothing
  | otherwise = viaNonEmpty WL.mkWikiLinkFromSlugs (Slug.decodeSlug <$> T.splitOn "/" s)

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
  guard (not (T.null t))
  pure t

readIntArg :: Text -> Maybe (Map Text Aeson.Value) -> Maybe Int
readIntArg k margs = do
  args <- margs
  Aeson.Number n <- Map.lookup k args
  pure (truncate (toRational n))

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
