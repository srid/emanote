{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Emanote.Model.Type where

import Commonmark.Extensions.WikiLink qualified as WL
import Data.Aeson qualified as Aeson
import Data.Default (Default (def))
import Data.IxSet.Typed ((@=))
import Data.IxSet.Typed qualified as Ix
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime)
import Data.Tree (Forest)
import Data.UUID (UUID)
import Ema.CLI qualified
import Emanote.Model.Link.Rel (IxRel)
import Emanote.Model.Link.Rel qualified as Rel
import Emanote.Model.Note (
  IxNote,
  Note,
  noteHasFeed,
 )
import Emanote.Model.Note qualified as N
import Emanote.Model.SData (IxSData, SData, sdataRoute)
import Emanote.Model.StaticFile (
  IxStaticFile,
  StaticFile (StaticFile),
  StaticFileInfo,
 )
import Emanote.Model.Stork.Index qualified as Stork
import Emanote.Model.Task (IxTask)
import Emanote.Model.Task qualified as Task
import Emanote.Model.Title qualified as Tit
import Emanote.Pandoc.Markdown.Syntax.HashTag qualified as HT
import Emanote.Pandoc.Renderer (EmanotePandocRenderers)
import Emanote.Route (FileType (AnyExt), LMLRoute, R)
import Emanote.Route qualified as R
import Emanote.Route.SiteRoute.Type (SiteRoute)
import Emanote.Source.Loc (Loc)
import Heist.Extra.TemplateState (TemplateState)
import Optics.Core (Prism')
import Optics.Operators ((%~), (.~), (^.))
import Optics.TH (makeLenses)
import Relude

data Status = Status_Loading | Status_Ready
  deriving stock (Eq, Show)

data ModelT encF = Model
  { _modelStatus :: Status
  , _modelLayers :: Set Loc
  , _modelEmaCLIAction :: Ema.CLI.Action
  , _modelRoutePrism :: encF (Prism' FilePath SiteRoute)
  , _modelPandocRenderers :: EmanotePandocRenderers Model LMLRoute
  -- ^ Dictates how exactly to render `Pandoc` to Heist nodes.
  , _modelCompileTailwind :: Bool
  , _modelInstanceID :: UUID
  -- ^ An unique ID for this process's model. ID changes across processes.
  , _modelNotes :: IxNote
  , _modelRels :: IxRel
  , _modelSData :: IxSData
  , _modelStaticFiles :: IxStaticFile
  , _modelTasks :: IxTask
  -- ^ A tree (forest) of all notes, based on their folder hierarchy.
  , _modelHeistTemplate :: TemplateState
  , _modelStorkIndex :: Stork.IndexVar
  , _modelFolgezettelTree :: Forest R.LMLRoute
  -- ^ Folgezettel tree computed once for each update to model.
  }
  deriving stock (Generic)

type Model = ModelT Identity

{- | A bare version of `Model` that is managed by the Ema app.

 The only difference is that this one has no `RouteEncoder`.
-}
type ModelEma = ModelT (Const ())

deriving stock instance Generic ModelEma

deriving stock instance Generic Model

makeLenses ''ModelT

withoutRoutePrism :: Model -> (Prism' FilePath SiteRoute, ModelEma)
withoutRoutePrism model@Model {..} =
  let _modelRoutePrism = Const ()
   in (runIdentity $ model ^. modelRoutePrism, Model {..})

withRoutePrism :: Prism' FilePath SiteRoute -> ModelEma -> Model
withRoutePrism enc Model {..} =
  let _modelRoutePrism = Identity enc
   in Model {..}

emptyModel :: Set Loc -> Ema.CLI.Action -> EmanotePandocRenderers Model LMLRoute -> Bool -> UUID -> Stork.IndexVar -> ModelEma
emptyModel layers act ren ctw instanceId storkVar =
  Model
    { _modelStatus = Status_Loading
    , _modelLayers = layers
    , _modelEmaCLIAction = act
    , _modelRoutePrism = Const ()
    , _modelPandocRenderers = ren
    , _modelCompileTailwind = ctw
    , _modelInstanceID = instanceId
    , -- Inject a placeholder `index.md` to account for the use case of emanote
      -- being run on an empty directory.
      _modelNotes = Ix.empty & injectRoot
    , _modelRels = Ix.empty
    , _modelSData = Ix.empty
    , _modelStaticFiles = Ix.empty
    , _modelTasks = Ix.empty
    , _modelHeistTemplate = def
    , _modelStorkIndex = storkVar
    , _modelFolgezettelTree = mempty
    }

modelReadyForView :: ModelT f -> ModelT f
modelReadyForView =
  modelStatus .~ Status_Ready

-- | Are we running in live server, or statically generated website?
inLiveServer :: Model -> Bool
inLiveServer = Ema.CLI.isLiveServer . _modelEmaCLIAction

modelInsertNote :: Note -> ModelT f -> ModelT f
modelInsertNote note =
  modelNotes
    %~ ( Ix.updateIx r note
          -- Insert folder placeholder automatically for ancestor paths
          >>> injectAncestors (N.noteAncestors note)
          >>> dropRedundantAncestor r
       )
      >>> modelRels
    %~ updateIxMulti r (Rel.noteRels note)
      >>> modelTasks
    %~ updateIxMulti r (Task.noteTasks note)
  where
    r = note ^. N.noteRoute

{- | If a placeholder route was added already, but the newly added note is a
 non-Markdown, removce that markdown placeholder route.
-}
dropRedundantAncestor :: LMLRoute -> IxNote -> IxNote
dropRedundantAncestor recentNoteRoute ns =
  case recentNoteRoute of
    R.LMLRoute_Md _ -> ns
    R.LMLRoute_Org r ->
      case N.lookupNotesByRoute (R.LMLRoute_Md $ coerce r) ns of
        Nothing -> ns
        Just placeholderNote -> Ix.deleteIx (placeholderNote ^. N.noteRoute) ns

injectAncestors :: [N.RAncestor] -> IxNote -> IxNote
injectAncestors ancs' =
  case nonEmpty ancs' of
    Nothing ->
      injectRoot
    Just ancs ->
      flip (foldr injectAncestor) ancs

-- Restore folder placeholder, if $folder.md gets deleted (with $folder/*.md still present)
-- TODO: If $k.md is the only file in its parent, delete unnecessary ancestors
restoreAncestor :: Maybe N.RAncestor -> IxNote -> IxNote
restoreAncestor =
  maybe injectRoot injectAncestor

injectRoot :: IxNote -> IxNote
injectRoot =
  injectAncestorPlaceholder R.indexRoute

injectAncestor :: N.RAncestor -> IxNote -> IxNote
injectAncestor =
  injectAncestorPlaceholder . N.unRAncestor

injectAncestorPlaceholder :: R ext -> IxNote -> IxNote
injectAncestorPlaceholder r ns =
  case resolveLmlRouteIfExists ns r of
    Just _ -> ns
    Nothing ->
      Ix.updateIx (R.defaultLmlRoute r) (N.ancestorPlaceholderNote $ coerce r) ns

modelDeleteNote :: LMLRoute -> ModelT f -> ModelT f
modelDeleteNote k model =
  model
    & modelNotes
    %~ ( Ix.deleteIx k
          >>> restoreAncestor (N.RAncestor <$> mFolderR)
       )
      & modelRels
    %~ deleteIxMulti k
      & modelTasks
    %~ deleteIxMulti k
  where
    -- If the note being deleted is $folder.md *and* folder/ has .md files, this
    -- will be `Just folderRoute`.
    mFolderR = do
      let folderR = R.withLmlRoute coerce k
      guard $ N.hasChildNotes folderR $ model ^. modelNotes
      pure folderR

-- | Like `Ix.updateIx`, but works for multiple items.
updateIxMulti ::
  (Ix.IsIndexOf ix ixs, Ix.Indexable ixs a) =>
  ix ->
  Ix.IxSet ixs a ->
  Ix.IxSet ixs a ->
  Ix.IxSet ixs a
updateIxMulti r new rels =
  let old = rels @= r
      deleteMany = foldr Ix.delete
   in new `Ix.union` (rels `deleteMany` old)

-- | Like `Ix.deleteIx`, but works for multiple items
deleteIxMulti ::
  (Ix.Indexable ixs a, Ix.IsIndexOf ix ixs) =>
  ix ->
  Ix.IxSet ixs a ->
  Ix.IxSet ixs a
deleteIxMulti r rels =
  let candidates = Ix.toList $ Ix.getEQ r rels
   in flipfoldl' Ix.delete rels candidates

modelLookupStaticFile :: FilePath -> ModelT f -> Maybe StaticFile
modelLookupStaticFile fp m = do
  r :: R.R 'AnyExt <- R.mkRouteFromFilePath fp
  Ix.getOne $ Ix.getEQ r $ m ^. modelStaticFiles

modelInsertStaticFile :: UTCTime -> R.R 'AnyExt -> FilePath -> Maybe StaticFileInfo -> ModelT f -> ModelT f
modelInsertStaticFile t r fp mInfo =
  modelStaticFiles %~ Ix.updateIx r staticFile
  where
    staticFile = StaticFile r fp t mInfo

modelDeleteStaticFile :: R.R 'AnyExt -> ModelT f -> ModelT f
modelDeleteStaticFile r =
  modelStaticFiles %~ Ix.deleteIx r

modelInsertData :: SData -> ModelT f -> ModelT f
modelInsertData v =
  modelSData %~ Ix.updateIx (v ^. sdataRoute) v

modelDeleteData :: R.R 'R.Yaml -> ModelT f -> ModelT f
modelDeleteData k =
  modelSData %~ Ix.deleteIx k

modelLookupNoteByRoute :: (R.LMLView, LMLRoute) -> ModelT f -> Maybe (R.LMLView, Note)
modelLookupNoteByRoute (view, r) (_modelNotes -> notes) = do
  note <- N.lookupNotesByRoute r notes
  case (view, noteHasFeed note) of
    (R.LMLView_Atom, True) -> pure (view, note)
    (R.LMLView_Atom, False) -> Nothing
    (_, _) -> pure (view, note)

-- | Like `modelLookupNoteByRoute`, but for `R.Html` routes only.
modelLookupNoteByRoute' :: LMLRoute -> ModelT f -> Maybe Note
modelLookupNoteByRoute' r =
  fmap snd . modelLookupNoteByRoute (R.LMLView_Html, r)

modelLookupNoteByHtmlRoute :: R 'R.Html -> ModelT f -> Rel.ResolvedRelTarget Note
modelLookupNoteByHtmlRoute r =
  Rel.resolvedRelTargetFromCandidates Nothing
    . N.lookupNotesByHtmlRoute r
    . _modelNotes

modelLookupFeedNoteByHtmlRoute :: R 'R.Xml -> ModelT f -> Maybe Note
modelLookupFeedNoteByHtmlRoute r model = case resolvedTarget of
  Rel.RRTFound note
    | noteHasFeed note -> pure note
    | otherwise -> Nothing
  _ -> Nothing
  where
    resolvedTarget =
      Rel.resolvedRelTargetFromCandidates Nothing
        $ N.lookupNotesByXmlRoute r
        $ _modelNotes model

modelLookupTitle :: LMLRoute -> ModelT f -> Tit.Title
modelLookupTitle r =
  maybe (Tit.fromRoute r) N._noteTitle . modelLookupNoteByRoute' r

-- Lookup the wiki-link and return its candidates in the model.
modelWikiLinkTargets :: WL.WikiLink -> Model -> [Either (R.LMLView, Note) StaticFile]
modelWikiLinkTargets wl model =
  let notes =
        Ix.toList
          $ (model ^. modelNotes)
          @= wl
      staticFiles =
        Ix.toList
          $ (model ^. modelStaticFiles)
          @= wl
   in fmap Right staticFiles <> fmap Left ((R.LMLView_Html,) <$> notes)

modelLookupStaticFileByRoute :: R 'AnyExt -> ModelT f -> Maybe StaticFile
modelLookupStaticFileByRoute r =
  Ix.getOne . Ix.getEQ r . _modelStaticFiles

modelTags :: ModelT f -> [(HT.Tag, [Note])]
modelTags =
  Ix.groupAscBy @HT.Tag . _modelNotes

modelNoteRels :: Model -> [Rel.Rel]
modelNoteRels =
  Ix.toList . _modelRels

modelNoteMetas :: Model -> Map LMLRoute (Tit.Title, LMLRoute, Aeson.Value)
modelNoteMetas model =
  Map.fromList
    $ Ix.toList (_modelNotes model)
    <&> \note ->
      (note ^. N.noteRoute, (note ^. N.noteTitle, note ^. N.noteRoute, note ^. N.noteMeta))

modelNoteErrors :: Model -> Map LMLRoute [Text]
modelNoteErrors model =
  Map.fromList
    $ flip mapMaybe (Ix.toList (_modelNotes model))
    $ \note -> do
      let errs = note ^. N.noteErrors
      guard $ not $ null errs
      pure (note ^. N.noteRoute, errs)

{- | Return the most suitable index LML route

  If index.org exist, use that. Otherwise, fallback to index.md.
-}
modelIndexRoute :: ModelT f -> LMLRoute
modelIndexRoute model = do
  resolveLmlRoute model R.indexRoute

resolveLmlRoute :: forall lmlType f. ModelT f -> R ('R.LMLType lmlType) -> LMLRoute
resolveLmlRoute model r =
  fromMaybe (R.defaultLmlRoute r) $ resolveLmlRouteIfExists (model ^. modelNotes) r

-- | Lookup a LML route, returning the less popular LML format if there are ambiguities.
resolveLmlRouteIfExists :: forall ext. IxNote -> R ext -> Maybe LMLRoute
resolveLmlRouteIfExists notes r = do
  -- TODO: Refactor using `[minBound..maxBound] :: [LML]`
  note <-
    asum
      [ N.lookupNotesByRoute (R.LMLRoute_Org $ coerce r) notes
      , N.lookupNotesByRoute (R.LMLRoute_Md $ coerce r) notes
      ]
  pure $ note ^. N.noteRoute

{- | Return the route to parent folder (unless indexRoute is passed).

  This will return the existing note (.org or .md) if possible. Otherwise
  fallback to .md even if missing.
-}
parentLmlRoute :: Model -> R.LMLRoute -> Maybe R.LMLRoute
parentLmlRoute model r = do
  pr <- do
    let lmlR = R.lmlRouteCase r
    -- Root index do not have a parent folder.
    guard $ lmlR /= Left R.indexRoute && lmlR /= Right R.indexRoute
    -- Consider the index route as parent folder for all
    -- top-level notes.
    pure $ fromMaybe R.indexRoute $ R.withLmlRoute R.routeParent r
  pure $ resolveLmlRoute model . coerce $ pr
