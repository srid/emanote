{- | Reverse index from external source files to the notes whose parsed
 AST depends on them.

 Today only @.lua@ filter files are tracked. Edges are keyed by the
 path as written in a note's @pandoc.filters@ frontmatter — *not* by
 the resolved absolute path — so a filter referenced before it
 exists on disk still gets an edge: when the file is later created,
 the same key matches and the dependent is re-parsed.

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
import Data.Set qualified as Set
import Emanote.Route qualified as R
import Relude

newtype SourceDependencies = SourceDependencies
  { sdLuaDeps :: Map FilePath (Set R.LMLRoute)
  -- ^ Each key is a filter path as written in the dependent note's
  -- @pandoc.filters@ frontmatter — typically a layer-relative form
  -- like @"filters/list-table.lua"@. Both resolved and unresolved
  -- references live in this map; see the module haddock.
  }
  deriving stock (Eq, Show, Generic)

emptyDependencies :: SourceDependencies
emptyDependencies = SourceDependencies mempty

{- | Notes that reference the given filter path. The lookup key is the
requested form (per 'sdLuaDeps') — not a resolved absolute path.
-}
dependentsOnLua :: FilePath -> SourceDependencies -> Set R.LMLRoute
dependentsOnLua fp = maybeToMonoid . Map.lookup fp . sdLuaDeps

{- | Replace the set of edges originating at @note@ with @paths@,
removing any stale edges from a previous parse of the same note.
-}
setLuaDeps :: R.LMLRoute -> [FilePath] -> SourceDependencies -> SourceDependencies
setLuaDeps note paths (removeNote note -> SourceDependencies m) =
  SourceDependencies $ foldl' addEdge m paths
  where
    addEdge :: Map FilePath (Set R.LMLRoute) -> FilePath -> Map FilePath (Set R.LMLRoute)
    addEdge m' fp = Map.insertWith Set.union fp (one note) m'

-- | Drop every edge that points to @note@.
removeNote :: R.LMLRoute -> SourceDependencies -> SourceDependencies
removeNote note (SourceDependencies m) =
  SourceDependencies $ Map.mapMaybe shrink m
  where
    shrink s =
      let s' = Set.delete note s
       in if Set.null s' then Nothing else Just s'
