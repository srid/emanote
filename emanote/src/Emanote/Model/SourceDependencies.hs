{-# LANGUAGE RecordWildCards #-}

{- | Reverse index from external source files to the notes whose parsed
 or rendered output depends on them.

 Today only @.lua@ filter files are tracked, and they are tracked by
 phase: parse-time filters live in their own edge map separate from
 render-time HTML filters. The two phases do not fail in the same
 way (a render-only filter change does not require re-parsing source)
 so the data shape preserves the option for a phase-specific refresh
 path even though the patcher today re-parses on either kind.

 Edges are keyed by the path as written in the note-local Lua filter
 declaration — *not* by the resolved absolute path — so a filter
 referenced before it exists on disk still gets an edge: when the
 file is later created, the same key matches and the dependent is
 re-parsed.

 The value at each edge carries the dependent note's source
 @(Loc, FilePath)@ — exactly what @parseAndInsert@ in
 "Emanote.Source.Patch" needs to re-read and re-parse the note. By
 storing the rebuild data in the edge itself, the refresh path is a
 simple fold over @parseAndInsert@ and needs nothing from the model
 (no @lookupNotesByRoute@ → @noteSource@ recovery dance).

 A future second edge kind (e.g. cascaded @index.yaml@ files
 affecting a note's effective filter list) still has to extend
 'setLuaDeps' / 'removeNote' so deleted notes don't leak edges in
 the new map. See [#721].
-}
module Emanote.Model.SourceDependencies (
  SourceDependencies,
  emptyDependencies,
  dependentsOnLua,
  setLuaDeps,
  removeNote,
) where

import Data.Map.Strict qualified as Map
import Emanote.Route qualified as R
import Emanote.Source.Loc (Loc)
import Relude

type LuaDepEdges = Map FilePath (Map R.LMLRoute (Loc, FilePath))

data SourceDependencies = SourceDependencies
  { sdParseLuaDeps :: LuaDepEdges
  -- ^ Filter paths declared under @pandoc.filters.parse@ (Markdown
  -- frontmatter) or @#+PANDOC_FILTERS:@ (Org). Editing one of these
  -- files invalidates the dependent note's *parse* output.
  , sdRenderLuaDeps :: LuaDepEdges
  -- ^ Filter paths declared under @pandoc.filters.render.html@. Editing
  -- one of these invalidates only the dependent note's *render* output;
  -- the parsed AST stays valid.
  }
  deriving stock (Eq, Show, Generic)

emptyDependencies :: SourceDependencies
emptyDependencies = SourceDependencies mempty mempty

{- | Notes that reference the given filter path through *any* phase. The
lookup key is the requested form — not a resolved absolute path.

The patcher today re-parses on either kind, so the union is what it
needs. A future render-only refresh path can switch to reading
'sdRenderLuaDeps' directly.
-}
dependentsOnLua :: FilePath -> SourceDependencies -> Map R.LMLRoute (Loc, FilePath)
dependentsOnLua fp SourceDependencies {..} =
  Map.union
    (maybeToMonoid $ Map.lookup fp sdParseLuaDeps)
    (maybeToMonoid $ Map.lookup fp sdRenderLuaDeps)

{- | Replace the set of edges originating at @note@ with @parsePaths@ and
@renderPaths@, removing any stale edges from a previous parse of the
same note. The note's source @(Loc, FilePath)@ is recorded on every
new edge.
-}
setLuaDeps :: R.LMLRoute -> (Loc, FilePath) -> [FilePath] -> [FilePath] -> SourceDependencies -> SourceDependencies
setLuaDeps note src parsePaths renderPaths (removeNote note -> SourceDependencies parseMap renderMap) =
  SourceDependencies (foldl' addEdge parseMap parsePaths) (foldl' addEdge renderMap renderPaths)
  where
    addEdge :: LuaDepEdges -> FilePath -> LuaDepEdges
    addEdge m fp = Map.insertWith Map.union fp (one (note, src)) m

-- | Drop every edge that points to @note@ in either phase.
removeNote :: R.LMLRoute -> SourceDependencies -> SourceDependencies
removeNote note (SourceDependencies parseMap renderMap) =
  SourceDependencies (shrinkAll parseMap) (shrinkAll renderMap)
  where
    shrinkAll = Map.mapMaybe shrink
    shrink inner =
      let inner' = Map.delete note inner
       in if Map.null inner' then Nothing else Just inner'
