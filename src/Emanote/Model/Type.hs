{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Emanote.Model.Type where

import Control.Lens.Operators as Lens ((%~), (.~), (^.))
import Control.Lens.TH (makeLenses)
import Data.Default (Default (def))
import Data.IxSet.Typed ((@=))
import qualified Data.IxSet.Typed as Ix
import Data.Time (UTCTime)
import Data.Tree (Tree)
import Ema (Slug)
import qualified Ema.Helper.PathTree as PathTree
import Emanote.Model.Link.Rel (IxRel)
import qualified Emanote.Model.Link.Rel as Rel
import Emanote.Model.Note
  ( IxNote,
    Note,
  )
import qualified Emanote.Model.Note as N
import Emanote.Model.SData (IxSData, SData, sdataRoute)
import Emanote.Model.StaticFile
  ( IxStaticFile,
    StaticFile (StaticFile),
  )
import qualified Emanote.Model.Title as Tit
import qualified Emanote.Pandoc.Markdown.Syntax.HashTag as HT
import qualified Emanote.Pandoc.Markdown.Syntax.WikiLink as WL
import Emanote.Route (FileType (AnyExt), LMLRoute, R)
import qualified Emanote.Route as R
import Heist.Extra.TemplateState (TemplateState)

data Status = Status_Loading | Status_Ready
  deriving (Eq, Show)

data Model = Model
  { _modelStatus :: Status,
    _modelNotes :: IxNote,
    _modelRels :: IxRel,
    _modelSData :: IxSData,
    _modelStaticFiles :: IxStaticFile,
    -- TODO: Avoid incremental building (which is complex), and compute this on
    -- demand like `modelTags`? Use memoization to avoid repeat computation if
    -- model hasn't changed. NOTE: Recomputation on single-file change will be
    -- O(n), so maybe this is not a good idea.
    -- TODO: Should modelNav be removed in favour of inferring nav from modelGraph?
    _modelNav :: [Tree Slug],
    _modelHeistTemplate :: TemplateState
  }

makeLenses ''Model

emptyModel :: Model
emptyModel =
  Model Status_Loading Ix.empty Ix.empty Ix.empty mempty mempty def

modelReadyForView :: Model -> Model
modelReadyForView =
  modelStatus .~ Status_Ready

modelInsertNote :: Note -> Model -> Model
modelInsertNote note =
  modelNotes
    %~ ( Ix.updateIx r note
           -- Insert folder placeholder automatically for ancestor paths
           >>> flip (foldr injectAncestor) (N.noteAncestors note)
       )
    >>> modelRels
    %~ replaceNoteRels
    >>> modelNav
    %~ PathTree.treeInsertPath (R.unRoute . R.lmlRouteCase $ r)
  where
    r = note ^. N.noteRoute
    replaceNoteRels rels =
      let old = rels @= r
          new = Rel.noteRels note
          deleteMany = foldr Ix.delete
       in new `Ix.union` (rels `deleteMany` old)

injectAncestor :: N.RAncestor -> IxNote -> IxNote
injectAncestor ancestor ns =
  let lmlR = R.liftLMLRoute @('R.LMLType 'R.Md) . coerce $ N.unRAncestor ancestor
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
      & modelNav
    %~ maybe (PathTree.treeDeletePath (R.unRoute . R.lmlRouteCase $ k)) (const id) mFolderR
  where
    -- If the note being deleted is $folder.md *and* folder/ has .md files, this
    -- will be `Just folderRoute`.
    mFolderR = do
      let folderR = coerce $ R.lmlRouteCase k
      guard $ N.hasChildNotes folderR $ model ^. modelNotes
      pure folderR
    restoreFolderPlaceholder =
      injectAncestor . N.RAncestor
    deleteIxMulti :: LMLRoute -> IxRel -> IxRel
    deleteIxMulti r rels =
      let candidates = Ix.toList $ Ix.getEQ r rels
       in foldl' (flip Ix.delete) rels candidates

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
