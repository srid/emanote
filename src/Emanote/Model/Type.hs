{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Emanote.Model.Type where

import Control.Lens.Operators as Lens ((%~), (^.))
import Control.Lens.TH (makeLenses)
import Data.IxSet.Typed ((@+), (@=))
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
    staticFileRoute,
  )
import qualified Emanote.Pandoc.Markdown.Syntax.WikiLink as WL
import Emanote.Route (FileType (AnyExt), LinkableLMLRoute, LinkableRoute, R)
import qualified Emanote.Route as R
import Heist.Extra.TemplateState (TemplateState, newTemplateState)
import qualified Text.Pandoc.Definition as B

data Model = Model
  { _modelNotes :: IxNote,
    _modelRels :: IxRel,
    _modelSData :: IxSData,
    _modelStaticFiles :: IxStaticFile,
    _modelNav :: [Tree Slug],
    _modelHeistTemplate :: TemplateState
  }

makeLenses ''Model

emptyModel :: MonadIO m => m Model
emptyModel =
  Model Ix.empty Ix.empty Ix.empty mempty mempty
    <$> newTemplateState

modelInsertNote :: Note -> Model -> Model
modelInsertNote note =
  modelNotes
    %~ ( Ix.updateIx r note
           >>> flip (foldr injectAncestor) (N.noteAncestors note)
       )
    >>> modelRels
    %~ replaceNoteRels
    >>> modelNav
    %~ PathTree.treeInsertPath (R.unRoute . R.linkableLMLRouteCase $ r)
  where
    r = note ^. N.noteRoute
    replaceNoteRels rels =
      let old = rels @= r
          new = Rel.noteRels note
          deleteMany = foldr Ix.delete
       in new `Ix.union` (rels `deleteMany` old)
    -- Automatically add notes corresponding to parent paths if a
    -- corresponding .md does not exist.
    injectAncestor :: N.RAncestor -> IxNote -> IxNote
    injectAncestor ancestor ns =
      let lmlR = R.liftLinkableLMLRoute @('R.LMLType 'R.Md) . coerce $ N.unRAncestor ancestor
       in case nonEmpty (N.lookupNotesByRoute lmlR ns) of
            Just _ -> ns
            Nothing -> Ix.updateIx lmlR (N.placeHolderNote lmlR) ns

modelDeleteNote :: LinkableLMLRoute -> Model -> Model
modelDeleteNote k =
  modelNotes %~ Ix.deleteIx k
    >>> modelRels %~ Ix.deleteIx k
    -- FIXME: If removing folder.md, this shoudn't remove children!
    -- Use `treeDeleteLeafPath` to ensure we remove only leaf paths.
    >>> modelNav %~ PathTree.treeDeletePath (R.unRoute . R.linkableLMLRouteCase $ k)

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

modelLookupNoteByRoute :: LinkableLMLRoute -> Model -> Maybe Note
modelLookupNoteByRoute r (_modelNotes -> notes) =
  N.singleNote (N.lookupNotesByRoute r notes)

modelLookupNoteByHtmlRoute :: R 'R.Html -> Model -> Maybe Note
modelLookupNoteByHtmlRoute r (_modelNotes -> notes) =
  N.singleNote (N.lookupNotesByHtmlRoute r notes)

modelLookupTitle :: LinkableLMLRoute -> Model -> Text
modelLookupTitle r =
  maybe (R.routeBaseName $ R.linkableLMLRouteCase r) N.noteTitle . modelLookupNoteByRoute r

modelResolveWikiLink :: WL.WikiLink -> Model -> [LinkableRoute]
modelResolveWikiLink wl model =
  let noteRoutes =
        fmap (R.liftLinkableRoute . R.linkableLMLRouteCase . (^. N.noteRoute)) . Ix.toList $
          (model ^. modelNotes) @= wl
      staticRoutes =
        fmap (R.liftLinkableRoute . (^. staticFileRoute)) . Ix.toList $
          (model ^. modelStaticFiles) @= wl
   in staticRoutes <> noteRoutes

modelLookupBacklinks :: LinkableRoute -> Model -> [(LinkableLMLRoute, [B.Block])]
modelLookupBacklinks r model =
  let backlinks = Ix.toList $ (model ^. modelRels) @+ Rel.unresolvedRelsTo r
   in backlinks <&> \rel ->
        (rel ^. Rel.relFrom, rel ^. Rel.relCtx)

modelLookupStaticFileByRoute :: R 'AnyExt -> Model -> Maybe StaticFile
modelLookupStaticFileByRoute r model = do
  Ix.getOne . Ix.getEQ r . _modelStaticFiles $ model
