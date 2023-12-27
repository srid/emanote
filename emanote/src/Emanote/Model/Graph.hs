module Emanote.Model.Graph where

import Commonmark.Extensions.WikiLink qualified as WL
import Data.IxSet.Typed ((@+), (@=))
import Data.IxSet.Typed qualified as Ix
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Tree (Forest, Tree (Node))
import Emanote.Model.Calendar qualified as Calendar
import Emanote.Model.Link.Rel qualified as Rel
import Emanote.Model.Link.Resolve qualified as Resolve
import Emanote.Model.Meta (lookupRouteMeta)
import Emanote.Model.Note qualified as MN
import Emanote.Model.Note qualified as N
import Emanote.Model.Type (Model, modelIndexRoute, modelNotes, modelRels, parentLmlRoute)
import Emanote.Route qualified as R
import Optics.Operators as Lens ((^.))
import Relude hiding (empty)
import Text.Pandoc.Definition qualified as B

-- TODO: Do breadth-first instead of depth-first
modelFolgezettelAncestorTree :: Model -> R.LMLRoute -> Forest R.LMLRoute
modelFolgezettelAncestorTree model =
  fst . usingState mempty . go
  where
    go :: (MonadState (Set R.LMLRoute) m) => R.LMLRoute -> m (Forest R.LMLRoute)
    go r =
      fmap catMaybes . forM (folgezettelParentsFor model r) $ \parentR -> do
        gets (parentR `Set.member`) >>= \case
          True -> pure Nothing -- already visited
          False -> do
            modify $ Set.insert parentR
            sub <- go parentR
            pure $ Just $ Node parentR sub

folgezettelParentsFor :: Model -> R.LMLRoute -> [R.LMLRoute]
folgezettelParentsFor model r = do
  let folgezettelBacklinks =
        backlinkRels r model
          & filter (isFolgezettel . (^. Rel.relTo))
          <&> (^. Rel.relFrom)
      -- Handle reverse folgezettel links (`#[[..]]`) here
      folgezettelFrontlinks =
        frontlinkRels r model
          & mapMaybe (lookupNoteByWikiLink model <=< selectReverseFolgezettel . (^. Rel.relTo))
      -- Folders are automatically made a folgezettel
      folgezettelFolder =
        maybeToList $ do
          guard $ folderFolgezettelEnabledFor model r
          parentLmlRoute model r
      folgezettelParents =
        ordNub
          $ mconcat
            [ folgezettelBacklinks
            , folgezettelFrontlinks
            , folgezettelFolder
            ]
   in folgezettelParents
  where
    isFolgezettel = \case
      Rel.URTWikiLink (WL.WikiLinkBranch, _wl) ->
        True
      _ ->
        False
    selectReverseFolgezettel :: Rel.UnresolvedRelTarget -> Maybe WL.WikiLink
    selectReverseFolgezettel = \case
      Rel.URTWikiLink (WL.WikiLinkTag, wl) -> Just wl
      _ -> Nothing

folgezettelChildrenFor :: Model -> R.LMLRoute -> [R.LMLRoute]
folgezettelChildrenFor model r = do
  let folgezettelBacklinks =
        backlinkRels r model
          & filter (isReverseFolgezettel . (^. Rel.relTo))
          <&> (^. Rel.relFrom)
      folgezettelFrontlinks =
        frontlinkRels r model
          & mapMaybe (lookupNoteByWikiLink model <=< selectFolgezettel . (^. Rel.relTo))
      -- Folders are automatically made a folgezettel
      folgezettelFolderChildren :: [R.LMLRoute] =
        -- TODO: document inverse semantics of folder-folgezettel. Also update the uplink tree implementation.
        if folderFolgezettelEnabledFor model r
          then
            if r == modelIndexRoute model
              then
                let notes = Ix.toList $ (model ^. modelNotes) @= (Nothing :: Maybe (R.R 'R.Folder))
                 in flip mapMaybe notes $ \note -> do
                      let childR = note ^. MN.noteRoute
                      guard $ childR /= r -- Exclude index.md being a children of itself.
                      pure childR
              else
                let folderR :: R.R 'R.Folder = R.withLmlRoute coerce r
                    notes = Ix.toList $ (model ^. modelNotes) @= Just folderR
                    rs = notes <&> (^. MN.noteRoute)
                 in rs
          else []
      folgezettelChildren =
        ordNub
          $ mconcat
            [ folgezettelBacklinks
            , folgezettelFrontlinks
            , folgezettelFolderChildren
            ]
   in folgezettelChildren
  where
    isReverseFolgezettel = \case
      Rel.URTWikiLink (WL.WikiLinkTag, _wl) ->
        True
      _ ->
        False
    selectFolgezettel :: Rel.UnresolvedRelTarget -> Maybe WL.WikiLink
    selectFolgezettel = \case
      Rel.URTWikiLink (WL.WikiLinkBranch, wl) -> Just wl
      _ -> Nothing

{- | Returns a tree of all folgezettel notes, starting from the given route.

If a note has two parents, that note will be positioned in *both* the sub-trees.

Cycles are discarded.

Returns the tree, while considering unvisited (disconnected) notes as sibling trees.

`fromRoute` node itself is ignored fro the returned tree.
-}
folgezettelTreesFrom :: Model -> R.LMLRoute -> Forest R.LMLRoute
folgezettelTreesFrom model fromRoute =
  snd $ usingState (mempty :: Forest R.LMLRoute) $ do
    let loop fromR rs = do
          let (tree@(Node _ children), unvisited) = usingState rs $ go Set.empty fromR
          if fromR == fromRoute
            then modify (<> children) -- ignore `fromRoute` itself
            else modify (<> one tree)
          case nonEmpty (Set.toList unvisited) of
            Nothing -> pass
            Just ne -> loop (head ne) unvisited
    loop fromRoute allRoutes
  where
    allRoutes = Set.fromList $ fmap N._noteRoute $ Ix.toList $ model ^. modelNotes
    go :: (MonadState (Set R.LMLRoute) m) => Set R.LMLRoute -> R.LMLRoute -> m (Tree R.LMLRoute)
    go visitedRoutes route
      | route `Set.member` visitedRoutes = do
          modify $ Set.delete route
          pure $ Node route []
      -- TODO: Favour folder children first?
      -- TODO: Does this respect `order` key?
      | otherwise = do
          modify $ Set.delete route
          -- Filter children by not visited.
          -- FIXME: How do we we keep to allowedRoutes while allowing multiple parents?
          -- allowedRoutes <- get
          let children = folgezettelChildrenFor model route -- & filter (`Set.member` allowedRoutes)
          cs <- traverse (go (Set.insert route visitedRoutes)) children
          pure $ Node route cs

folderFolgezettelEnabledFor :: Model -> R.LMLRoute -> Bool
folderFolgezettelEnabledFor model r =
  lookupRouteMeta defaultValue ("emanote" :| ["folder-folgezettel"]) r model
  where
    -- We don't treat the top-level folder as folgezettel, to support the "flat
    -- list of notes" use case (popularized by original neuron).
    defaultValue = r /= modelIndexRoute model

lookupNoteByWikiLink :: Model -> WL.WikiLink -> Maybe R.LMLRoute
lookupNoteByWikiLink model wl = do
  (_, note) <- leftToMaybe <=< getFound $ Resolve.resolveWikiLinkMustExist model wl
  pure $ note ^. MN.noteRoute
  where
    getFound :: Rel.ResolvedRelTarget a -> Maybe a
    getFound = \case
      Rel.RRTFound x -> Just x
      _ -> Nothing

modelLookupBacklinks :: R.LMLRoute -> Model -> [(R.LMLRoute, NonEmpty [B.Block])]
modelLookupBacklinks r model =
  sortOn (Calendar.backlinkSortKey model . fst)
    $ groupNE
    $ backlinkRels r model
    <&> \rel ->
      (rel ^. Rel.relFrom, rel ^. Rel.relCtx)
  where
    groupNE :: forall a b. (Ord a) => [(a, b)] -> [(a, NonEmpty b)]
    groupNE =
      Map.toList . foldl' f Map.empty
      where
        f :: Map a (NonEmpty b) -> (a, b) -> Map a (NonEmpty b)
        f m (x, y) =
          case Map.lookup x m of
            Nothing -> Map.insert x (one y) m
            Just ys -> Map.insert x (ys <> one y) m

-- | Rels pointing *to* this route
backlinkRels :: R.LMLRoute -> Model -> [Rel.Rel]
backlinkRels r model =
  let allPossibleLinks = Rel.unresolvedRelsTo $ toModelRoute r
   in Ix.toList $ (model ^. modelRels) @+ allPossibleLinks
  where
    toModelRoute = R.ModelRoute_LML R.LMLView_Html

-- | Rels pointing *from* this route
frontlinkRels :: R.LMLRoute -> Model -> [Rel.Rel]
frontlinkRels r model =
  maybeToMonoid $ do
    pure $ Ix.toList $ (model ^. modelRels) @= r
