{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

{- | Pure query helpers backing the MCP tools.

MCP-independent by design: this module has no @dpella/mcp@ imports and no
URI-scheme layer — only the model query layer. Tools live next door in
"Emanote.MCP.Tools" and adapt these helpers to MCP wire types. Mirrors the
'Emanote.MCP.Catalog' / 'Emanote.MCP.Handlers' split phase 2 introduced for
resources.
-}
module Emanote.MCP.ToolCatalog (
  NoteMatch (..),
  ResolveResult (..),
  findNotes,
  getBacklinks,
  resolveWikilink,
) where

import Commonmark.Extensions.WikiLink qualified as WL
import Data.Aeson (ToJSON (..), (.=))
import Data.Aeson qualified as Aeson
import Data.IxSet.Typed qualified as Ix
import Data.Text qualified as T
import Emanote.Model (Model)
import Emanote.Model qualified as M
import Emanote.Model.Graph qualified as G
import Emanote.Model.Link.Rel qualified as Rel
import Emanote.Model.Link.Resolve qualified as Resolve
import Emanote.Model.Note qualified as N
import Emanote.Model.StaticFile qualified as SF
import Emanote.Model.Title qualified as Tit
import Emanote.Route qualified as R
import Network.URI.Slug qualified as Slug
import Optics.Operators ((^.))
import Relude

-- ---------------------------------------------------------------------------
-- Result shapes
-- ---------------------------------------------------------------------------

{- | A single note hit returned by 'findNotes' and 'getBacklinks'.

The @path@ is the note's source-relative path (e.g. @guide/mcp.md@) —
the same path used as a key in the JSON metadata export. Clients read
the underlying file through their own filesystem tools; MCP no longer
serves note bodies.
-}
data NoteMatch = NoteMatch
  { path :: Text
  , title :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON NoteMatch where
  toJSON NoteMatch {path, title} =
    Aeson.object
      [ "path" .= path
      , "title" .= title
      ]

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
-- Queries
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
  pure $ case Resolve.resolveWikiLinkMustExist model fromR wl of
    Rel.RRTFound (Left (_, note)) -> ResolvedNote (noteMatchOf note)
    Rel.RRTFound (Right sf) -> ResolvedStatic (staticFilePath sf)
    Rel.RRTMissing -> UnresolvedMissing
    Rel.RRTAmbiguous cs -> UnresolvedAmbiguous $ toList $ candidate <$> cs
  where
    candidate = \case
      Left (_, note) -> Left (noteMatchOf note)
      Right sf -> Right (staticFilePath sf)
    staticFilePath sf = toText $ R.encodeRoute (sf ^. SF.staticFileRoute)

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Parse a slash-separated wikilink target (e.g. "foo/bar") into a 'WL.WikiLink'.
parseWikiLinkText :: Text -> Maybe WL.WikiLink
parseWikiLinkText s
  | T.null s = Nothing
  | otherwise = viaNonEmpty WL.mkWikiLinkFromSlugs (Slug.decodeSlug <$> T.splitOn "/" s)
