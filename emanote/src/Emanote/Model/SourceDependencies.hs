{- | Reverse index from external source files to the notes whose parsed
 or rendered output depends on them.

 Today only @.lua@ filter files are tracked. Edges are keyed by the
 path as written in the note-local Lua filter declaration — *not* by
 the resolved absolute path — so a filter referenced before it
 exists on disk still gets an edge: when the file is later created,
 the same key matches and the dependent is re-parsed.

 The value at each edge carries the dependent note's source
 @(Loc, FilePath)@ — exactly what @parseAndInsert@ in
 "Emanote.Source.Patch" needs to re-read and re-parse the note. By
 storing the rebuild data in the edge itself, the refresh path is a
 simple fold over @parseAndInsert@ and needs nothing from the model
 (no @lookupNotesByRoute@ → @noteSource@ recovery dance).

 A future second edge kind (e.g. cascaded @index.yaml@ files
affecting a note's effective filter list) cannot be added as a
sibling field without also extending the mutation API:
 'removeNote' currently iterates 'sdLuaDeps' alone, so a new field
 would silently leak edges for deleted notes. See [#721].
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

newtype SourceDependencies = SourceDependencies
  { sdLuaDeps :: Map FilePath (Map R.LMLRoute (Loc, FilePath))
  -- ^ Outer key is a filter path as written in the dependent note's
  -- note-local Lua filter declaration — typically a layer-relative
  -- form like @"filters/list-table.lua"@. Inner key is the dependent
  -- note's route; inner value is its on-disk source — enough to drive
  -- the rebuild without consulting the model. Both resolved and
  -- unresolved references live here; see the module haddock.
  }
  deriving stock (Eq, Show, Generic)

emptyDependencies :: SourceDependencies
emptyDependencies = SourceDependencies mempty

{- | Notes that reference the given filter path, paired with each
dependent's on-disk source location. The lookup key is the requested
form (per 'sdLuaDeps') — not a resolved absolute path.
-}
dependentsOnLua :: FilePath -> SourceDependencies -> Map R.LMLRoute (Loc, FilePath)
dependentsOnLua fp = maybeToMonoid . Map.lookup fp . sdLuaDeps

{- | Replace the set of edges originating at @note@ with @paths@,
removing any stale edges from a previous parse of the same note. The
note's source @(Loc, FilePath)@ is recorded on every new edge.
-}
setLuaDeps :: R.LMLRoute -> (Loc, FilePath) -> [FilePath] -> SourceDependencies -> SourceDependencies
setLuaDeps note src paths (removeNote note -> SourceDependencies m) =
  SourceDependencies $ foldl' addEdge m paths
  where
    addEdge :: Map FilePath (Map R.LMLRoute (Loc, FilePath)) -> FilePath -> Map FilePath (Map R.LMLRoute (Loc, FilePath))
    addEdge m' fp = Map.insertWith Map.union fp (one (note, src)) m'

-- | Drop every edge that points to @note@.
removeNote :: R.LMLRoute -> SourceDependencies -> SourceDependencies
removeNote note (SourceDependencies m) =
  SourceDependencies $ Map.mapMaybe shrink m
  where
    shrink inner =
      let inner' = Map.delete note inner
       in if Map.null inner' then Nothing else Just inner'
