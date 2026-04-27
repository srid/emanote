{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Emanote.Site.Model where

import Commonmark.Extensions.WikiLink qualified as WL
import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.Default (Default (def))
import Data.Time (UTCTime)
import Data.Tree (Forest)
import Data.UUID (UUID)
import Emanote.Model.Link.Rel (IxRel)
import Emanote.Model.Link.Rel qualified as Rel
import Emanote.Model.Meta qualified as Meta
import Emanote.Model.Note (IxNote, Note)
import Emanote.Model.SData (IxSData, SData)
import Emanote.Model.StaticFile (IxStaticFile, StaticFile, StaticFileInfo)
import Emanote.Model.Stork.Index qualified as Stork
import Emanote.Model.Task (IxTask)
import Emanote.Model.Title qualified as Tit
import Emanote.Model.Type qualified as Core
import Emanote.Pandoc.Markdown.Syntax.HashTag qualified as HT
import Emanote.Pandoc.Renderer (EmanotePandocRenderers)
import Emanote.Route (FileType (AnyExt), LMLRoute, R)
import Emanote.Route qualified as R
import Emanote.Route.SiteRoute.Type (SiteRoute)
import Emanote.Source.Loc (Loc)
import Heist.Extra.TemplateState (TemplateState)
import Optics.Core (Lens', Prism', (%))
import Optics.Operators ((%~), (^.))
import Optics.TH (makeLenses)
import Relude

type Status = Core.Status

pattern Status_Loading :: Status
pattern Status_Loading = Core.Status_Loading

pattern Status_Ready :: Status
pattern Status_Ready = Core.Status_Ready

{-# COMPLETE Status_Loading, Status_Ready #-}

data ModelT encF = Model
  { _modelCore :: Core.Model
  , _modelIsLiveServer :: Bool
  , _modelRoutePrism :: encF (Prism' FilePath SiteRoute)
  , _modelPandocRenderers :: EmanotePandocRenderers Model LMLRoute
  -- ^ Dictates how exactly to render `Pandoc` to Heist nodes.
  , _modelCompileTailwind :: Bool
  , _modelInstanceID :: UUID
  -- ^ An unique ID for this process's model. ID changes across processes.
  , _modelHeistTemplate :: TemplateState
  , _modelStorkIndex :: Stork.IndexVar
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

modelStatus :: Lens' (ModelT f) Status
modelStatus = modelCore % Core.modelStatus

modelLayers :: Lens' (ModelT f) (Set Loc)
modelLayers = modelCore % Core.modelLayers

modelNotes :: Lens' (ModelT f) IxNote
modelNotes = modelCore % Core.modelNotes

modelRels :: Lens' (ModelT f) IxRel
modelRels = modelCore % Core.modelRels

modelSData :: Lens' (ModelT f) IxSData
modelSData = modelCore % Core.modelSData

modelStaticFiles :: Lens' (ModelT f) IxStaticFile
modelStaticFiles = modelCore % Core.modelStaticFiles

modelTasks :: Lens' (ModelT f) IxTask
modelTasks = modelCore % Core.modelTasks

modelFolgezettelTree :: Lens' (ModelT f) (Forest R.LMLRoute)
modelFolgezettelTree = modelCore % Core.modelFolgezettelTree

withoutRoutePrism :: Model -> (Prism' FilePath SiteRoute, ModelEma)
withoutRoutePrism model@Model {..} =
  let _modelRoutePrism = Const ()
   in (runIdentity $ model ^. modelRoutePrism, Model {..})

withRoutePrism :: Prism' FilePath SiteRoute -> ModelEma -> Model
withRoutePrism enc Model {..} =
  let _modelRoutePrism = Identity enc
   in Model {..}

emptyModel :: Set Loc -> Bool -> EmanotePandocRenderers Model LMLRoute -> Bool -> UUID -> Stork.IndexVar -> ModelEma
emptyModel layers isLiveServer ren ctw instanceId storkVar =
  Model
    { _modelCore = Core.emptyModel layers
    , _modelIsLiveServer = isLiveServer
    , _modelRoutePrism = Const ()
    , _modelPandocRenderers = ren
    , _modelCompileTailwind = ctw
    , _modelInstanceID = instanceId
    , _modelHeistTemplate = def
    , _modelStorkIndex = storkVar
    }

modelReadyForView :: ModelT f -> ModelT f
modelReadyForView =
  modelCore %~ Core.modelReadyForView

-- | Are we running in live server, or statically generated website?
inLiveServer :: Model -> Bool
inLiveServer = _modelIsLiveServer

modelInsertNote :: Note -> ModelT f -> ModelT f
modelInsertNote note =
  modelCore %~ Core.modelInsertNote note

modelDeleteNote :: LMLRoute -> ModelT f -> ModelT f
modelDeleteNote r =
  modelCore %~ Core.modelDeleteNote r

modelLookupStaticFile :: FilePath -> ModelT f -> Maybe StaticFile
modelLookupStaticFile fp =
  Core.modelLookupStaticFile fp . _modelCore

modelInsertStaticFile :: UTCTime -> R 'AnyExt -> FilePath -> Maybe StaticFileInfo -> ModelT f -> ModelT f
modelInsertStaticFile t r fp mInfo =
  modelCore %~ Core.modelInsertStaticFile t r fp mInfo

modelDeleteStaticFile :: R 'AnyExt -> ModelT f -> ModelT f
modelDeleteStaticFile r =
  modelCore %~ Core.modelDeleteStaticFile r

modelInsertData :: SData -> ModelT f -> ModelT f
modelInsertData v =
  modelCore %~ Core.modelInsertData v

modelDeleteData :: R 'R.Yaml -> ModelT f -> ModelT f
modelDeleteData k =
  modelCore %~ Core.modelDeleteData k

modelLookupSData :: R 'R.Yaml -> ModelT f -> Maybe SData
modelLookupSData r =
  Core.modelLookupSData r . _modelCore

modelLookupNoteByRoute :: (R.LMLView, LMLRoute) -> ModelT f -> Maybe (R.LMLView, Note)
modelLookupNoteByRoute route =
  Core.modelLookupNoteByRoute route . _modelCore

modelLookupNoteByRoute' :: LMLRoute -> ModelT f -> Maybe Note
modelLookupNoteByRoute' r =
  Core.modelLookupNoteByRoute' r . _modelCore

modelResolveLinkBase :: ModelT f -> LMLRoute -> Maybe (R 'R.Folder)
modelResolveLinkBase model =
  Core.modelResolveLinkBase (_modelCore model)

modelLookupNoteByHtmlRoute :: R 'R.Html -> ModelT f -> Rel.ResolvedRelTarget Note
modelLookupNoteByHtmlRoute r =
  Core.modelLookupNoteByHtmlRoute r . _modelCore

modelLookupFeedNoteByHtmlRoute :: R 'R.Xml -> ModelT f -> Maybe Note
modelLookupFeedNoteByHtmlRoute r =
  Core.modelLookupFeedNoteByHtmlRoute r . _modelCore

modelLookupTitle :: LMLRoute -> ModelT f -> Tit.Title
modelLookupTitle r =
  Core.modelLookupTitle r . _modelCore

modelWikiLinkTargets :: WL.WikiLink -> Model -> [Either (R.LMLView, Note) StaticFile]
modelWikiLinkTargets wl =
  Core.modelWikiLinkTargets wl . _modelCore

modelLookupStaticFileByRoute :: R 'AnyExt -> ModelT f -> Maybe StaticFile
modelLookupStaticFileByRoute r =
  Core.modelLookupStaticFileByRoute r . _modelCore

modelTags :: ModelT f -> [(HT.Tag, [Note])]
modelTags =
  Core.modelTags . _modelCore

modelNoteRels :: ModelT f -> [Rel.Rel]
modelNoteRels =
  Core.modelNoteRels . _modelCore

modelNoteMetas :: ModelT f -> Map LMLRoute (Tit.Title, LMLRoute, Aeson.Value)
modelNoteMetas =
  Core.modelNoteMetas . _modelCore

modelNoteErrors :: ModelT f -> Map LMLRoute [Text]
modelNoteErrors =
  Core.modelNoteErrors . _modelCore

modelIndexRoute :: ModelT f -> LMLRoute
modelIndexRoute =
  Core.modelIndexRoute . _modelCore

resolveLmlRoute :: forall lmlType f. ModelT f -> R ('R.LMLType lmlType) -> LMLRoute
resolveLmlRoute model =
  Core.resolveLmlRoute (_modelCore model)

parentLmlRoute :: ModelT f -> R.LMLRoute -> Maybe R.LMLRoute
parentLmlRoute model =
  Core.parentLmlRoute (_modelCore model)

lookupRouteMeta :: (FromJSON a) => a -> NonEmpty Text -> R.LMLRoute -> ModelT f -> a
lookupRouteMeta x k r =
  Meta.lookupRouteMeta x k r . _modelCore

getEffectiveRouteMeta :: R.LMLRoute -> ModelT f -> Aeson.Value
getEffectiveRouteMeta r =
  Meta.getEffectiveRouteMeta r . _modelCore

getEffectiveRouteMetaWith :: Aeson.Value -> R.LMLRoute -> ModelT f -> Aeson.Value
getEffectiveRouteMetaWith frontmatter r =
  Meta.getEffectiveRouteMetaWith frontmatter r . _modelCore

cascadeYamlErrors :: ModelT f -> R.LMLRoute -> [Text]
cascadeYamlErrors model =
  Meta.cascadeYamlErrors (_modelCore model)
