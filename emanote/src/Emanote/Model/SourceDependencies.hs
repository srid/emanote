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

newtype SourceDependencies = SourceDependencies
  { sdLuaDeps :: Map FilePath (Set R.LMLRoute)
  -- ^ Reverse map from a Lua filter file to the notes that reference
  -- it. The key is the layer-resolved path produced by
  -- 'Emanote.Source.Loc.locResolve' — i.e. the layer's @-L@ value
  -- joined to the user's @pandoc.filters@ spec via @\<\/\>@. That's
  -- working-directory-relative if the user passed @-L docs@ and rooted
  -- at @\/@ if they passed @-L \/abs\/path@; the index doesn't care
  -- which form, only that it matches what the @LuaFilter@ refresh
  -- handler in 'Emanote.Source.Patch' computes from the unionmount
  -- event so producer and consumer collide on the same string.
  --
  -- The value is the set of 'R.LMLRoute's whose cached
  -- 'Text.Pandoc.Definition.Pandoc' AST includes the output of that
  -- filter; a filter referenced by N notes has one entry with a set of
  -- size N. Empty sets are pruned by 'removeNote' so an absent key and
  -- an empty value are equivalent (use 'maybeToMonoid' on lookup) —
  -- keeps invalidation walks linear in the actual dependent count
  -- rather than total filter count.
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
