{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Emanote.Model.Type where

import Data.Aeson qualified as Aeson
import Data.Default (Default (def))
import Data.IxSet.Typed ((@=))
import Data.IxSet.Typed qualified as Ix
import Data.Map.Strict qualified as Map
import Data.Some (Some)
import Data.Time (UTCTime)
import Data.Tree (Tree)
import Data.Tree.Path qualified as PathTree
import Data.UUID (UUID)
import Ema.CLI qualified
import Ema.Route.Encoder (RouteEncoder)
import Emanote.Model.Link.Rel (IxRel)
import Emanote.Model.Link.Rel qualified as Rel
import Emanote.Model.Note
  ( IxNote,
    Note,
  )
import Emanote.Model.Note qualified as N
import Emanote.Model.SData (IxSData, SData, sdataRoute)
import Emanote.Model.StaticFile
  ( IxStaticFile,
    StaticFile (StaticFile),
  )
import Emanote.Model.Task (IxTask)
import Emanote.Model.Task qualified as Task
import Emanote.Model.Title qualified as Tit
import Emanote.Pandoc.Markdown.Syntax.HashTag qualified as HT
import Emanote.Pandoc.Markdown.Syntax.WikiLink qualified as WL
import Emanote.Pandoc.Renderer
import Emanote.Route (FileType (AnyExt), LMLRoute, R)
import Emanote.Route qualified as R
import Emanote.Route.SiteRoute.Type
import Emanote.Source.Loc (Loc)
import Heist.Extra.TemplateState (TemplateState)
import Network.URI.Slug (Slug)
import Optics.Operators ((%~), (.~), (^.))
import Optics.TH (makeLenses)
import Relude

data Status = Status_Loading | Status_Ready
  deriving stock (Eq, Show)

data Model = Model
  { _modelStatus :: Status,
    _modelLayers :: Set Loc,
    _modelEmaCLIAction :: Some Ema.CLI.Action,
    _modelRouteEncoder :: RouteEncoder Model SiteRoute,
    -- | Dictates how exactly to render `Pandoc` to Heist nodes.
    _modelPandocRenderers :: EmanotePandocRenderers Model LMLRoute,
    -- | An unique ID for this process's model. ID changes across processes.
    _modelInstanceID :: UUID,
    _modelNotes :: IxNote,
    _modelRels :: IxRel,
    _modelSData :: IxSData,
    _modelStaticFiles :: IxStaticFile,
    _modelTasks :: IxTask,
    _modelNav :: [Tree Slug],
    _modelHeistTemplate :: TemplateState
  }

makeLenses ''Model

emptyModel :: Set Loc -> Some Ema.CLI.Action -> RouteEncoder Model SiteRoute -> EmanotePandocRenderers Model LMLRoute -> UUID -> Model
emptyModel layers act enc ren instanceId =
  Model Status_Loading layers act enc ren instanceId Ix.empty Ix.empty Ix.empty Ix.empty mempty mempty def

modelReadyForView :: Model -> Model
modelReadyForView =
  modelStatus .~ Status_Ready

-- | Are we running in live server, or statically generated website?
inLiveServer :: Model -> Bool
inLiveServer = Ema.CLI.isLiveServer . _modelEmaCLIAction

modelInsertNote :: Note -> Model -> Model
modelInsertNote note =
  modelNotes
    %~ ( Ix.updateIx r note
           -- Insert folder placeholder automatically for ancestor paths
           >>> flip (foldr injectAncestor) (N.noteAncestors note)
       )
    >>> modelRels
    %~ updateIxMulti r (Rel.noteRels note)
    >>> modelTasks
    %~ updateIxMulti r (Task.noteTasks note)
    >>> modelNav
    %~ PathTree.treeInsertPath (R.withLmlRoute R.unRoute $ r)
  where
    r = note ^. N.noteRoute

injectAncestor :: N.RAncestor -> IxNote -> IxNote
injectAncestor ancestor ns =
  -- FIXME: Md?
  let lmlR = R.LMLRoute_Md . coerce $ N.unRAncestor ancestor
   in case N.lookupNotesByRoute lmlR ns of
        Just _ -> ns
        Nothing -> Ix.updateIx lmlR (N.ancestorPlaceholderNote lmlR) ns

modelDeleteNote :: LMLRoute -> Model -> Model
modelDeleteNote k model =
  model & modelNotes
    %~ ( Ix.deleteIx k
           -- Restore folder placeholder, if $folder.md gets deleted (with $folder/*.md still present)
           -- TODO: If $k.md is the only file in its parent, delete unnecessary ancestors
           >>> maybe id restoreFolderPlaceholder mFolderR
       )
    & modelRels
    %~ deleteIxMulti k
    & modelTasks
    %~ deleteIxMulti k
    & modelNav
    %~ maybe (PathTree.treeDeletePath (R.withLmlRoute R.unRoute $ k)) (const id) mFolderR
  where
    -- If the note being deleted is $folder.md *and* folder/ has .md files, this
    -- will be `Just folderRoute`.
    mFolderR = do
      let folderR = R.withLmlRoute coerce k
      guard $ N.hasChildNotes folderR $ model ^. modelNotes
      pure folderR
    restoreFolderPlaceholder =
      injectAncestor . N.RAncestor

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

modelLookupStaticFile :: FilePath -> Model -> Maybe StaticFile
modelLookupStaticFile fp m = do
  r :: R.R 'AnyExt <- R.mkRouteFromFilePath fp
  Ix.getOne $ Ix.getEQ r $ m ^. modelStaticFiles

modelInsertStaticFile :: UTCTime -> R.R 'AnyExt -> FilePath -> Model -> Model
modelInsertStaticFile t r fp =
  modelStaticFiles %~ Ix.updateIx r staticFile
  where
    staticFile = StaticFile r fp t

modelDeleteStaticFile :: R.R 'AnyExt -> Model -> Model
modelDeleteStaticFile r =
  modelStaticFiles %~ Ix.deleteIx r

modelInsertData :: SData -> Model -> Model
modelInsertData v =
  modelSData %~ Ix.updateIx (v ^. sdataRoute) v

modelDeleteData :: R.R 'R.Yaml -> Model -> Model
modelDeleteData k =
  modelSData %~ Ix.deleteIx k

modelLookupNoteByRoute :: LMLRoute -> Model -> Maybe Note
modelLookupNoteByRoute r (_modelNotes -> notes) =
  N.lookupNotesByRoute r notes

modelLookupNoteByHtmlRoute :: R 'R.Html -> Model -> Rel.ResolvedRelTarget Note
modelLookupNoteByHtmlRoute r =
  Rel.resolvedRelTargetFromCandidates
    . N.lookupNotesByHtmlRoute r
    . _modelNotes

modelLookupTitle :: LMLRoute -> Model -> Tit.Title
modelLookupTitle r =
  maybe (Tit.fromRoute r) N._noteTitle . modelLookupNoteByRoute r

-- Lookup the wiki-link and return its candidates in the model.
modelWikiLinkTargets :: WL.WikiLink -> Model -> [Either Note StaticFile]
modelWikiLinkTargets wl model =
  let notes =
        Ix.toList $
          (model ^. modelNotes) @= wl
      staticFiles =
        Ix.toList $
          (model ^. modelStaticFiles) @= wl
   in fmap Right staticFiles <> fmap Left notes

modelLookupStaticFileByRoute :: R 'AnyExt -> Model -> Maybe StaticFile
modelLookupStaticFileByRoute r =
  Ix.getOne . Ix.getEQ r . _modelStaticFiles

modelTags :: Model -> [(HT.Tag, [Note])]
modelTags =
  Ix.groupAscBy @HT.Tag . _modelNotes

modelNoteRels :: Model -> [Rel.Rel]
modelNoteRels =
  Ix.toList . _modelRels

modelNoteMetas :: Model -> Map LMLRoute (Tit.Title, LMLRoute, Aeson.Value)
modelNoteMetas model =
  Map.fromList $
    Ix.toList (_modelNotes model) <&> \note ->
      (note ^. N.noteRoute, (note ^. N.noteTitle, note ^. N.noteRoute, note ^. N.noteMeta))

modelNoteErrors :: Model -> Map LMLRoute [Text]
modelNoteErrors model =
  Map.fromList $
    flip mapMaybe (Ix.toList (_modelNotes model)) $ \note -> do
      let errs = note ^. N.noteErrors
      guard $ not $ null errs
      pure (note ^. N.noteRoute, errs)
