{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Emabook.Model where

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
import Emabook.Model.Note
  ( IxNote,
    Note (Note),
    SelfRef (SelfRef),
    noteRoute,
    noteTitle,
  )
import Emabook.Model.Rel (IxRel)
import qualified Emabook.Model.Rel as Rel
import Emabook.Model.SData (IxSData, SData (SData))
import Emabook.Route (MarkdownRoute)
import qualified Emabook.Route as R
import qualified Emabook.Route.Ext as Ext
import qualified Emabook.Route.WikiLinkTarget as WL
import qualified Emabook.Template as T
import Text.Pandoc.Definition (Pandoc (..))
import qualified Text.Pandoc.Definition as B

-- | This is our Ema "model" -- the app state used to generate our site.
--
-- It contains the list of all markdown files, parsed as Pandoc AST.
data Model = Model
  { _modelNotes :: IxNote,
    _modelRels :: IxRel,
    _modelData :: IxSData,
    _modelNav :: [Tree Slug],
    _modelHeistTemplate :: T.TemplateState
  }

makeLenses ''Model

instance Default Model where
  def = Model Ix.empty Ix.empty Ix.empty mempty (Left $ one "Heist state not yet loaded")

modelLookup :: MarkdownRoute -> Model -> Maybe Note
modelLookup k =
  Ix.getOne . Ix.getEQ k . _modelNotes

modelLookupRouteByWikiLink :: WL.WikiLinkTarget -> Model -> [MarkdownRoute]
modelLookupRouteByWikiLink wl model =
  -- TODO: Also lookup wiki links to *directories* without an associated zettel.
  -- Eg: my [[Public Post Ideas]]
  fmap (^. noteRoute) . Ix.toList $ (model ^. modelNotes) @= SelfRef wl

modelLookupBacklinks :: MarkdownRoute -> Model -> [(MarkdownRoute, NonEmpty [B.Block])]
modelLookupBacklinks r model =
  let refsToSelf =
        Set.fromList $
          (Left <$> toList (WL.allowedWikiLinkTargets r))
            <> [Right r]
      backlinks = Ix.toList $ (model ^. modelRels) @+ toList refsToSelf
   in backlinks <&> \rel ->
        (rel ^. Rel.relFrom, rel ^. Rel.relCtx)

modelLookupTitle :: MarkdownRoute -> Model -> Text
modelLookupTitle r =
  maybe (R.routeFileBase r) noteTitle . modelLookup r

modelInsertData :: R.Route Ext.Yaml -> Aeson.Value -> Model -> Model
modelInsertData r v =
  modelData %~ Ix.updateIx r (SData v r)

modelDeleteData :: R.Route Ext.Yaml -> Model -> Model
modelDeleteData k =
  modelData %~ Ix.deleteIx k

modelInsert :: MarkdownRoute -> (Aeson.Value, Pandoc) -> Model -> Model
modelInsert k v =
  modelNotes %~ Ix.updateIx k note
    >>> modelRels %~ (Ix.deleteIx k >>> Ix.insertList (Rel.extractRels note))
    >>> modelNav %~ PathTree.treeInsertPath (R.unRoute k)
  where
    note = Note (snd v) (fst v) k

modelDelete :: MarkdownRoute -> Model -> Model
modelDelete k =
  modelNotes %~ Ix.deleteIx k
    >>> modelRels %~ Ix.deleteIx k
    >>> modelNav %~ PathTree.treeDeletePath (R.unRoute k)

staticRoutes :: Model -> [MarkdownRoute]
staticRoutes (fmap (^. noteRoute) . Ix.toList . (^. modelNotes) -> mdRoutes) =
  mdRoutes
