{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.Model.Graph where

import Control.Lens.Operators as Lens ((^.))
import Data.IxSet.Typed ((@+), (@=))
import qualified Data.IxSet.Typed as Ix
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Tree (Forest, Tree (Node))
import qualified Emanote.Model.Calendar as Calendar
import qualified Emanote.Model.Link.Rel as Rel
import qualified Emanote.Model.Link.Resolve as Resolve
import qualified Emanote.Model.Note as MN
import Emanote.Model.Type (Model, modelRels)
import qualified Emanote.Pandoc.Markdown.Syntax.WikiLink as WL
import qualified Emanote.Route as R
import Emanote.Route.ModelRoute (ModelRoute)
import Relude hiding (empty)
import qualified Text.Pandoc.Definition as B

-- TODO: Do breadth-first instead of depth-first
modelFolgezettelAncestorTree :: ModelRoute -> Model -> Forest R.LMLRoute
modelFolgezettelAncestorTree r0 model =
  fst $ flip runState mempty $ go r0
  where
    go :: MonadState (Set ModelRoute) m => ModelRoute -> m (Forest R.LMLRoute)
    go r = do
      let folgezettelBacklinks =
            backlinkRels r model
              & filter (isFolgezettel . (^. Rel.relTo))
              <&> (^. Rel.relFrom)
          -- Handle reverse folgezettel links here
          folgezettelFrontlinks =
            frontlinkRels r model
              & mapMaybe (lookupWikiLink <=< selectReverseFolgezettel . (^. Rel.relTo))
          -- Folders are automatically made a folgezettel
          parentFolderRoute = do
            pr <- do
              lmlR <- R.lmlRouteCase <$> leftToMaybe (R.modelRouteCase r)
              -- Root index do not have a parent folder.
              guard $ lmlR /= R.indexRoute
              -- Consider the index route as parent folder for all
              -- top-level notes.
              pure $ fromMaybe R.indexRoute $ R.routeParent lmlR
            pure $ R.liftLMLRoute @('R.LMLType 'R.Md) . coerce $ pr
          folgezettelParents =
            folgezettelBacklinks
              <> folgezettelFrontlinks
              <> maybeToList parentFolderRoute
      fmap catMaybes . forM folgezettelParents $ \parentR -> do
        let parentModelR = R.liftModelRoute . R.lmlRouteCase $ parentR
        gets (parentModelR `Set.member`) >>= \case
          True -> pure Nothing
          False -> do
            sub <- go parentModelR
            modify $ Set.insert parentModelR
            pure $ Just $ Node parentR sub
    isFolgezettel = \case
      Rel.URTWikiLink (WL.WikiLinkBranch, _wl) ->
        True
      _ ->
        False
    selectReverseFolgezettel :: Rel.UnresolvedRelTarget -> Maybe WL.WikiLink
    selectReverseFolgezettel = \case
      Rel.URTWikiLink (WL.WikiLinkTag, wl) -> Just wl
      _ -> Nothing
    lookupWikiLink :: WL.WikiLink -> Maybe R.LMLRoute
    lookupWikiLink wl = do
      note <- leftToMaybe <=< getFound $ Resolve.resolveWikiLinkMustExist model wl
      pure $ note ^. MN.noteRoute
    getFound :: Rel.ResolvedRelTarget a -> Maybe a
    getFound = \case
      Rel.RRTFound x -> Just x
      _ -> Nothing

modelLookupBacklinks :: ModelRoute -> Model -> [(R.LMLRoute, NonEmpty [B.Block])]
modelLookupBacklinks r model =
  sortOn (Calendar.backlinkSortKey model . fst) $
    groupNE $
      backlinkRels r model <&> \rel ->
        (rel ^. Rel.relFrom, rel ^. Rel.relCtx)
  where
    groupNE :: forall a b. Ord a => [(a, b)] -> [(a, NonEmpty b)]
    groupNE =
      Map.toList . foldl' f Map.empty
      where
        f :: Map a (NonEmpty b) -> (a, b) -> Map a (NonEmpty b)
        f m (x, y) =
          case Map.lookup x m of
            Nothing -> Map.insert x (one y) m
            Just ys -> Map.insert x (ys <> one y) m

backlinkRels :: ModelRoute -> Model -> [Rel.Rel]
backlinkRels r model =
  let allPossibleLinks = Rel.unresolvedRelsTo r
   in Ix.toList $ (model ^. modelRels) @+ allPossibleLinks

frontlinkRels :: ModelRoute -> Model -> [Rel.Rel]
frontlinkRels r model =
  fromMaybe mempty $ do
    lmlR <- leftToMaybe $ R.modelRouteCase r
    pure $ Ix.toList $ (model ^. modelRels) @= lmlR
