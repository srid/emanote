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
        if r == modelIndexRoute model
          then
            let notes = Ix.toList $ (model ^. modelNotes) @= (Nothing :: Maybe (R.R 'R.Folder))
             in flip mapMaybe notes $ \note -> do
                  let childR = note ^. MN.noteRoute
                  guard $ folderFolgezettelEnabledFor model childR
                  guard $ childR /= r -- Exclude index.md being a children of itself.
                  pure childR
          else
            let folderR :: R.R 'R.Folder = R.withLmlRoute coerce r
                notes = Ix.toList $ (model ^. modelNotes) @= Just folderR
                rs = filter (folderFolgezettelEnabledFor model) $ notes <&> (^. MN.noteRoute)
             in rs
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
-}
folgezettelTreeFrom :: Model -> R.LMLRoute -> Tree R.LMLRoute
folgezettelTreeFrom model = go Set.empty
  where
    go visitedRoutes route
      | route `Set.member` visitedRoutes = Node route []
      -- TODO: Favour folder children first?
      -- TODO: Does this respect `order` key?
      | otherwise = Node route $ folgezettelChildrenFor model route <&> go (Set.insert route visitedRoutes)

folderFolgezettelEnabledFor :: Model -> R.LMLRoute -> Bool
folderFolgezettelEnabledFor model r =
  lookupRouteMeta True ("emanote" :| ["folder-folgezettel"]) r model

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
