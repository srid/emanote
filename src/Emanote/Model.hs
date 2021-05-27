{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Emanote.Model where

import Control.Lens.Operators as Lens ((%~), (^.))
import Control.Lens.TH (makeLenses)
import qualified Data.Aeson as Aeson
import Data.Default (Default (..))
import Data.IxSet.Typed ((@+), (@=))
import qualified Data.IxSet.Typed as Ix
import qualified Data.Set as Set
import Data.Tree (Tree)
import Ema (Slug)
import qualified Ema.Helper.PathTree as PathTree
import Emanote.Model.Note
  ( IxNote,
    Note (Note),
    noteRoute,
    noteTitle,
  )
import Emanote.Model.Rel (IxRel)
import qualified Emanote.Model.Rel as Rel
import Emanote.Model.SData (IxSData, SData (SData), sdataRoute)
import Emanote.Model.SelfRef (SelfRef (SelfRef))
import Emanote.Model.StaticFile
import Emanote.Route (Route)
import qualified Emanote.Route as R
import Emanote.Route.Ext (FileType (AnyExt))
import qualified Emanote.Route.Ext as Ext
import Emanote.Route.SomeRoute
  ( SomeLMLRoute,
    SomeRoute,
    liftSomeRoute,
    someLMLRouteCase,
  )
import qualified Emanote.WikiLink as WL
import Heist.Extra.TemplateState (TemplateState)
import Text.Pandoc.Definition (Pandoc (..))
import qualified Text.Pandoc.Definition as B

-- TODO: Use https://hackage.haskell.org/package/data-lens-ixset-0.1.4/docs/Data-Lens-IxSet.html
data Model = Model
  { _modelNotes :: IxNote,
    _modelRels :: IxRel,
    -- TODO: Rename to `modelSData` and move parser function to SData.hs after
    -- newtyping Aeson.Value (and move the merge function in there as well)
    _modelSData :: IxSData,
    _modelStaticFiles :: IxStaticFile,
    _modelNav :: [Tree Slug],
    _modelHeistTemplate :: TemplateState
  }

makeLenses ''Model

instance Default Model where
  def = Model Ix.empty Ix.empty Ix.empty mempty mempty def

modelInsertNote :: SomeLMLRoute -> (Aeson.Value, Pandoc) -> Model -> Model
modelInsertNote k v =
  modelNotes %~ Ix.updateIx k note
    >>> modelRels
      %~ ( Ix.deleteIx k
             >>> Ix.insertList (Rel.extractRels note)
         )
    >>> modelNav %~ PathTree.treeInsertPath (R.unRoute . someLMLRouteCase $ k)
  where
    note = Note (snd v) (fst v) k

modelDeleteNote :: SomeLMLRoute -> Model -> Model
modelDeleteNote k =
  modelNotes %~ Ix.deleteIx k
    >>> modelRels %~ Ix.deleteIx k
    >>> modelNav %~ PathTree.treeDeletePath (R.unRoute . someLMLRouteCase $ k)

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

modelLookupNote :: SomeLMLRoute -> Model -> Maybe Note
modelLookupNote k =
  Ix.getOne . Ix.getEQ k . _modelNotes

modelLookupTitle :: SomeLMLRoute -> Model -> Text
modelLookupTitle r =
  maybe (R.routeBaseName $ someLMLRouteCase r) noteTitle . modelLookupNote r

modelLookupRouteByWikiLink :: WL.WikiLink -> Model -> [SomeRoute]
modelLookupRouteByWikiLink wl model =
  -- TODO: Also lookup wiki links to *directories* without an associated zettel.
  -- Eg: my [[Public Post Ideas]]
  --
  -- Could store `modelNoteDirs` and look that up.
  let noteRoutes =
        fmap (liftSomeRoute . someLMLRouteCase . (^. noteRoute)) . Ix.toList $
          (model ^. modelNotes) @= SelfRef wl
      staticRoutes =
        fmap (liftSomeRoute . (^. staticFileRoute)) . Ix.toList $
          (model ^. modelStaticFiles) @= SelfRef wl
   in staticRoutes <> noteRoutes

modelLookupBacklinks :: SomeRoute -> Model -> [(SomeLMLRoute, [B.Block])]
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
