{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Emanote.Model where

import Control.Lens.Operators as Lens ((%~), (^.))
import Control.Lens.TH (makeLenses)
import Data.Default (Default (..))
import Data.IxSet.Typed ((@+), (@=))
import qualified Data.IxSet.Typed as Ix
import qualified Data.Set as Set
import Data.Tree (Tree)
import Ema (Slug)
import qualified Ema.Helper.PathTree as PathTree
import Emanote.Model.Note
  ( IxNote,
    Note,
    noteRoute,
    noteTitle,
  )
import Emanote.Model.Rel (IxRel)
import qualified Emanote.Model.Rel as Rel
import Emanote.Model.SData (IxSData, SData, sdataRoute)
import Emanote.Model.StaticFile
  ( IxStaticFile,
    StaticFile (StaticFile),
    staticFileRoute,
  )
import Emanote.Route (Route)
import qualified Emanote.Route as R
import Emanote.Route.Ext (FileType (AnyExt))
import qualified Emanote.Route.Ext as Ext
import Emanote.Route.Linkable
  ( LinkableLMLRoute,
    LinkableRoute,
    liftLinkableRoute,
    someLinkableLMLRouteCase,
  )
import qualified Emanote.WikiLink as WL
import Heist.Extra.TemplateState (TemplateState)
import qualified Text.Pandoc.Definition as B

-- TODO: Use https://hackage.haskell.org/package/data-lens-ixset-0.1.4/docs/Data-Lens-IxSet.html
data Model = Model
  { _modelNotes :: IxNote,
    _modelRels :: IxRel,
    _modelSData :: IxSData,
    _modelStaticFiles :: IxStaticFile,
    _modelNav :: [Tree Slug],
    _modelHeistTemplate :: TemplateState
  }

makeLenses ''Model

instance Default Model where
  def = Model Ix.empty Ix.empty Ix.empty mempty mempty def

modelInsertNote :: Note -> Model -> Model
modelInsertNote note =
  modelNotes %~ Ix.updateIx r note
    >>> modelRels
      %~ ( Ix.deleteIx r
             >>> Ix.insertList (Rel.extractRels note)
         )
    >>> modelNav %~ PathTree.treeInsertPath (R.unRoute . someLinkableLMLRouteCase $ r)
  where
    r = note ^. noteRoute

modelDeleteNote :: LinkableLMLRoute -> Model -> Model
modelDeleteNote k =
  modelNotes %~ Ix.deleteIx k
    >>> modelRels %~ Ix.deleteIx k
    >>> modelNav %~ PathTree.treeDeletePath (R.unRoute . someLinkableLMLRouteCase $ k)

modelInsertStaticFile :: R.Route 'AnyExt -> FilePath -> Model -> Model
modelInsertStaticFile r fp =
  modelStaticFiles %~ Ix.updateIx r staticFile
  where
    staticFile = StaticFile r fp

modelDeleteStaticFile :: R.Route 'AnyExt -> Model -> Model
modelDeleteStaticFile r =
  modelStaticFiles %~ Ix.deleteIx r

modelInsertData :: SData -> Model -> Model
modelInsertData v =
  modelSData %~ Ix.updateIx (v ^. sdataRoute) v

modelDeleteData :: R.Route 'Ext.Yaml -> Model -> Model
modelDeleteData k =
  modelSData %~ Ix.deleteIx k

modelLookupNote :: LinkableLMLRoute -> Model -> Maybe Note
modelLookupNote k =
  Ix.getOne . Ix.getEQ k . _modelNotes

modelLookupTitle :: LinkableLMLRoute -> Model -> Text
modelLookupTitle r =
  maybe (R.routeBaseName $ someLinkableLMLRouteCase r) noteTitle . modelLookupNote r

modelLookupRouteByWikiLink :: WL.WikiLink -> Model -> [LinkableRoute]
modelLookupRouteByWikiLink wl model =
  -- TODO: Also lookup wiki links to *directories* without an associated zettel.
  -- Eg: my [[Public Post Ideas]]
  --
  -- Could store `modelNoteDirs` and look that up.
  let noteRoutes =
        fmap (liftLinkableRoute . someLinkableLMLRouteCase . (^. noteRoute)) . Ix.toList $
          (model ^. modelNotes) @= wl
      staticRoutes =
        fmap (liftLinkableRoute . (^. staticFileRoute)) . Ix.toList $
          (model ^. modelStaticFiles) @= wl
   in staticRoutes <> noteRoutes

modelLookupBacklinks :: LinkableRoute -> Model -> [(LinkableLMLRoute, [B.Block])]
modelLookupBacklinks r model =
  let refsToSelf =
        Set.fromList $
          (Left <$> toList (WL.allowedWikiLinks r))
            <> [Right r]
      backlinks = Ix.toList $ (model ^. modelRels) @+ toList refsToSelf
   in backlinks <&> \rel ->
        (rel ^. Rel.relFrom, rel ^. Rel.relCtx)

modelLookupStaticFile :: FilePath -> Model -> Maybe StaticFile
modelLookupStaticFile fp model = do
  flip modelLookupStaticFileByRoute model =<< R.mkRouteFromFilePath @'AnyExt fp

modelLookupStaticFileByRoute :: Route 'AnyExt -> Model -> Maybe StaticFile
modelLookupStaticFileByRoute r model = do
  Ix.getOne . Ix.getEQ r . _modelStaticFiles $ model
