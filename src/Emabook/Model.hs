{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Emabook.Model where

import Control.Lens.Operators as Lens ((%~), (^.))
import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Extra.Merge as AesonMerge
import Data.Default (Default (..))
import Data.IxSet.Typed ((@+), (@=))
import qualified Data.IxSet.Typed as Ix
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Tree (Tree)
import Ema (Slug)
import qualified Ema.Helper.PathTree as PathTree
import Emabook.Model.Note
  ( IxNote,
    Note (Note),
    SelfRef (SelfRef),
    noteMeta,
    noteRoute,
    noteTitle,
  )
import Emabook.Model.Rel (IxRel)
import qualified Emabook.Model.Rel as Rel
import Emabook.Model.SData (IxSData, SData (SData), sdataValue)
import Emabook.Route (MarkdownRoute)
import qualified Emabook.Route as R
import qualified Emabook.Route.Ext as Ext
import qualified Emabook.Route.WikiLinkTarget as WL
import qualified Emabook.Template as T
import Relude.Extra.Map (StaticMap (lookup))
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

lookupNoteMeta :: (Default a, FromJSON a) => a -> Text -> MarkdownRoute -> Model -> a
lookupNoteMeta x k r model =
  fromMaybe x $ do
    Aeson.Object obj <- pure $ modelComputeMeta r model
    resultToMaybe . Aeson.fromJSON =<< lookup k obj
  where
    resultToMaybe :: Aeson.Result b -> Maybe b
    resultToMaybe = \case
      Aeson.Error _ -> Nothing
      Aeson.Success b -> pure b

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

-- | Get the (final) metadata of a note, by merging it with the defaults
-- specified in parent routes all the way upto index.yaml.
modelComputeMeta :: MarkdownRoute -> Model -> Aeson.Value
modelComputeMeta mr model =
  -- NOTE: This should never return Aeson.Null as long there is an index.yaml
  -- TODO: Capture and warn of this invariant in user-friendly way.
  fromMaybe Aeson.Null $ do
    let defaultFiles = R.routeInits @Ext.Yaml (coerce mr)
    defaults <- nonEmpty $
      flip mapMaybe (toList defaultFiles) $ \r -> do
        v <- fmap (^. sdataValue) . Ix.getOne . Ix.getEQ r $ model ^. modelData
        guard $ v /= Aeson.Null
        pure v
    let finalDefault = NE.last $ NE.scanl1 mergeAeson defaults
    pure $
      fromMaybe finalDefault $ do
        frontmatter <- (^. noteMeta) <$> modelLookup mr model
        guard $ frontmatter /= Aeson.Null -- To not trip up AesonMerge
        pure $ mergeAeson finalDefault frontmatter
  where
    mergeAeson = AesonMerge.lodashMerge

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
