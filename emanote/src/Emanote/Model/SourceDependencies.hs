{-# LANGUAGE DeriveAnyClass #-}

{- | Reverse index from external source files to the notes that depend on
them, so that an edit to such a source can invalidate exactly the notes
whose parsed AST depends on it.

Today only @.lua@ filter files are tracked. The type is shaped as a
record so that future dependency edges (e.g. cascaded
@index.yaml@ files affecting a note's effective filter list) can be
added as additional fields without changing the existing API.
-}
module Emanote.Model.SourceDependencies (
  SourceDependencies,
  emptyDependencies,
  dependentsOnLua,
  setLuaDeps,
  removeNote,
) where

import Data.Aeson (ToJSON)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Emanote.Route qualified as R
import Relude

{- | Eager-parse-time edges only. A change to one of these source files
invalidates the cached 'Text.Pandoc.Definition.Pandoc' AST stored on
each 'Emanote.Model.Note.Note', which can only be recovered by
re-running the parser — hence the explicit reverse index.

Lazy-at-render-time inputs (yaml cascade via
'Emanote.Model.Meta.getEffectiveRouteMetaWith', Heist templates via
'_modelHeistTemplate') deliberately don't appear here: a fresh render
reads them straight out of the model after the change handler updates
it, so no per-note invalidation is needed.

When @pandoc.filters@ starts cascading from ancestor @index.yaml@
(tracked under #721), an @sdYamlDeps :: Map (R \'Yaml) (Set
R.LMLRoute)@ sibling field belongs here too — at that point a yaml
edit can change the effective filter list and so flips from
lazy-at-render to eager-at-parse for that one frontmatter key.
-}
newtype SourceDependencies = SourceDependencies
  { sdLuaDeps :: Map FilePath (Set R.LMLRoute)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

emptyDependencies :: SourceDependencies
emptyDependencies = SourceDependencies mempty

-- | Notes that reference the given absolute filter path.
dependentsOnLua :: FilePath -> SourceDependencies -> Set R.LMLRoute
dependentsOnLua fp = maybeToMonoid . Map.lookup fp . sdLuaDeps

{- | Replace the set of @.lua@ edges originating at @note@ with @paths@.
Removes any stale edges from a previous parse of the same note.
-}
setLuaDeps :: R.LMLRoute -> [FilePath] -> SourceDependencies -> SourceDependencies
setLuaDeps note paths =
  addEdges . removeNote note
  where
    addEdges sd = foldl' (insertEdge note) sd paths

-- | Drop every edge that points to @note@.
removeNote :: R.LMLRoute -> SourceDependencies -> SourceDependencies
removeNote note (SourceDependencies m) =
  SourceDependencies $ Map.mapMaybe shrink m
  where
    shrink s =
      let s' = Set.delete note s
       in if Set.null s' then Nothing else Just s'

insertEdge :: R.LMLRoute -> SourceDependencies -> FilePath -> SourceDependencies
insertEdge note (SourceDependencies m) fp =
  SourceDependencies $ Map.alter (Just . Set.insert note . maybeToMonoid) fp m
