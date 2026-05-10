{-# LANGUAGE RecordWildCards #-}

{- | Reverse index from external source files to the notes whose parsed or
 rendered output depends on them.

 Today only @.lua@ filter files are tracked. The patcher re-parses a note when
 any referenced filter changes, so this index records one relation from filter
 path to dependent note. Filter phase remains in 'PandocFilterDeclarations',
 where parse and render application actually diverge.

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
import Emanote.Model.Note.Filter qualified as NoteFilter
import Emanote.Route qualified as R
import Emanote.Source.Loc (Loc)
import Relude

type LuaDepEdges = Map FilePath (Map R.LMLRoute (Loc, FilePath))

newtype SourceDependencies = SourceDependencies
  { sdLuaDeps :: LuaDepEdges
  }
  deriving stock (Eq, Show, Generic)

emptyDependencies :: SourceDependencies
emptyDependencies = SourceDependencies mempty

-- | Notes that reference the given filter path through any phase.
dependentsOnLua :: FilePath -> SourceDependencies -> Map R.LMLRoute (Loc, FilePath)
dependentsOnLua fp SourceDependencies {..} =
  maybeToMonoid $ Map.lookup fp sdLuaDeps

{- | Replace the set of edges originating at @note@ with its declared Lua filter
paths, removing any stale edges from a previous parse of the same note. The
note's source @(Loc, FilePath)@ is recorded on every new edge.
-}
setLuaDeps :: R.LMLRoute -> (Loc, FilePath) -> NoteFilter.PandocFilterDeclarations -> SourceDependencies -> SourceDependencies
setLuaDeps note src declarations (removeNote note -> SourceDependencies luaMap) =
  SourceDependencies $ foldl' addEdge luaMap $ NoteFilter.pandocFilterDependencyPaths declarations
  where
    addEdge :: LuaDepEdges -> FilePath -> LuaDepEdges
    addEdge m fp = Map.insertWith Map.union fp (one (note, src)) m

-- | Drop every edge that points to @note@.
removeNote :: R.LMLRoute -> SourceDependencies -> SourceDependencies
removeNote note (SourceDependencies luaMap) =
  SourceDependencies $ shrinkAll luaMap
  where
    shrinkAll = Map.mapMaybe shrink
    shrink inner =
      let inner' = Map.delete note inner
       in if Map.null inner' then Nothing else Just inner'
