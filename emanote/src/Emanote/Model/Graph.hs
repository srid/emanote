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
import Emanote.Route.SiteRoute qualified as SR
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
          & mapMaybe
            ( \rel ->
                lookupNoteByWikiLink model (Just $ rel ^. Rel.relFrom) <=< selectReverseFolgezettel . (^. Rel.relTo) $ rel
            )
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
          & mapMaybe
            ( \rel ->
                lookupNoteByWikiLink model (Just $ rel ^. Rel.relFrom) <=< selectFolgezettel . (^. Rel.relTo) $ rel
            )
      -- Folders are automatically made a folgezettel
      folgezettelFolderChildren :: [R.LMLRoute] =
        if folderFolgezettelEnabledFor model r
          then
            let isIndexRoute = r == modelIndexRoute model
                parentR :: Maybe (R.R 'R.Folder) = if isIndexRoute then Nothing else Just (R.withLmlRoute coerce r)
                allowed route = not isIndexRoute || (route /= r) -- Exclude index.md from being a children of itself
                notes = Ix.toList $ (model ^. modelNotes) @= parentR
                rs = notes <&> (^. MN.noteRoute) & filter allowed
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
    -- Run in a state monad of unvisited routes, taking visited routes as argument.
    go :: (MonadState (Set R.LMLRoute) m) => Set R.LMLRoute -> R.LMLRoute -> m (Tree R.LMLRoute)
    go visitedRoutes route
      | route `Set.member` visitedRoutes = do
          modify $ Set.delete route
          pure $ Node route []
      | otherwise = do
          modify $ Set.delete route
          let children = folgezettelChildrenFor model route
          cs <- go (Set.insert route visitedRoutes) `traverse` children
          pure $ Node route cs

folderFolgezettelEnabledFor :: Model -> R.LMLRoute -> Bool
folderFolgezettelEnabledFor model r =
  lookupRouteMeta defaultValue ("emanote" :| ["folder-folgezettel"]) r model
  where
    -- We don't treat the top-level folder as folgezettel, to support the "flat
    -- list of notes" use case (popularized by original neuron).
    defaultValue = r /= modelIndexRoute model

lookupNoteByWikiLink :: Model -> Maybe R.LMLRoute -> WL.WikiLink -> Maybe R.LMLRoute
lookupNoteByWikiLink model mCurrentRoute wl = do
  (_, note) <- leftToMaybe <=< getFound $ Resolve.resolveWikiLinkMustExist model mCurrentRoute wl
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
      rels = Ix.toList $ (model ^. modelRels) @+ allPossibleLinks
   in -- Filter out duplicate backlinks (ones that were disambigutated about ambiguous wiki links)
      filter
        ( \rel -> isJust $ do
            SR.SiteRoute_ResourceRoute (SR.ResourceRoute_LML _ r') <-
              Rel.getResolved $ Resolve.resolveRel model rel
            guard $ r == r'
            pass
        )
        rels
  where
    toModelRoute = R.ModelRoute_LML R.LMLView_Html

-- | Rels pointing *from* this route
frontlinkRels :: R.LMLRoute -> Model -> [Rel.Rel]
frontlinkRels r model =
  maybeToMonoid $ do
    pure $ Ix.toList $ (model ^. modelRels) @= r
