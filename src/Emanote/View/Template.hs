{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.View.Template (render) where

import Control.Lens ((.~), (^.))
import qualified Data.Aeson.Types as Aeson
import Data.List (partition)
import qualified Data.List.NonEmpty as NE
import Data.Map.Syntax ((##))
import Data.WorldPeace.Union
  ( absurdUnion,
  )
import qualified Ema
import qualified Ema.Helper.PathTree as PathTree
import Emanote.Model (Model)
import qualified Emanote.Model as M
import qualified Emanote.Model.Calendar as Calendar
import qualified Emanote.Model.Graph as G
import qualified Emanote.Model.Meta as Meta
import qualified Emanote.Model.Note as MN
import qualified Emanote.Model.SData as SData
import qualified Emanote.Model.Title as Tit
import Emanote.Pandoc.BuiltinFilters (prepareNoteDoc)
import Emanote.Prelude (h)
import Emanote.Route (FileType (LMLType), LML (Md))
import qualified Emanote.Route as R
import qualified Emanote.Route.SiteRoute as SR
import Emanote.Route.SiteRoute.Class (indexLmlRoute)
import qualified Emanote.View.Common as C
import Emanote.View.Export (renderGraphExport)
import qualified Emanote.View.TagIndex as TagIndex
import qualified Emanote.View.TaskIndex as TaskIndex
import qualified Heist as H
import qualified Heist.Extra.Splices.List as Splices
import qualified Heist.Extra.Splices.Pandoc as Splices
import Heist.Extra.Splices.Pandoc.Ctx (emptyRenderCtx)
import qualified Heist.Extra.Splices.Tree as Splices
import qualified Heist.Interpreted as HI
import qualified Heist.Splices as Heist
import Relude
import qualified Shower
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))

render :: Model -> SR.SiteRoute -> Ema.Asset LByteString
render m (SR.SiteRoute sr) =
  let setErrorPageMeta =
        MN.noteMeta .~ SData.mergeAesons (withTemplateName "/templates/error" :| [withSiteTitle "Emanote Error"])
   in sr
        & absurdUnion
        `h` ( \(SR.MissingR urlPath) -> do
                let hereRoute = R.liftLMLRoute @('LMLType 'Md) . coerce $ R.decodeHtmlRoute urlPath
                    note404 =
                      MN.missingNote hereRoute (toText urlPath)
                        & setErrorPageMeta
                        & MN.noteTitle .~ "! Missing link"
                Ema.AssetGenerated Ema.Html $ renderLmlHtml m note404
            )
        `h` ( \(SR.AmbiguousR (urlPath, notes)) -> do
                let noteAmb =
                      MN.ambiguousNoteURL urlPath notes
                        & setErrorPageMeta
                        & MN.noteTitle .~ "! Ambiguous link"
                Ema.AssetGenerated Ema.Html $ renderLmlHtml m noteAmb
            )
        `h` renderResourceRoute m
        `h` renderVirtualRoute m

renderResourceRoute :: Model -> SR.ResourceRoute -> Ema.Asset LByteString
renderResourceRoute m =
  absurdUnion
    `h` ( \(r :: R.LMLRoute) -> do
            case M.modelLookupNoteByRoute r m of
              Just note ->
                Ema.AssetGenerated Ema.Html $ renderLmlHtml m note
              Nothing ->
                -- This should never be reached because decodeRoute looks up the model.
                error $ "Bad route: " <> show r
        )
    `h` ( \(_ :: R.StaticFileRoute, fpAbs :: FilePath) -> do
            Ema.AssetStatic fpAbs
        )

renderVirtualRoute :: Model -> SR.VirtualRoute -> Ema.Asset LByteString
renderVirtualRoute m =
  absurdUnion
    `h` ( \(SR.TagIndexR mtag) ->
            Ema.AssetGenerated Ema.Html $ TagIndex.renderTagIndex m mtag
        )
    `h` ( \SR.IndexR ->
            Ema.AssetGenerated Ema.Html $ renderSRIndex m
        )
    `h` ( \SR.ExportR ->
            Ema.AssetGenerated Ema.Other $ renderGraphExport m
        )
    `h` ( \SR.TasksR ->
            Ema.AssetGenerated Ema.Html $ TaskIndex.renderTasks m
        )

renderSRIndex :: Model -> LByteString
renderSRIndex model = do
  let meta = Meta.getIndexYamlMeta model
      tCtx = C.mkTemplateRenderCtx model indexLmlRoute meta
  C.renderModelTemplate model "templates/special/index" $ do
    C.commonSplices ($ emptyRenderCtx) model meta "Index"
    routeTreeSplice tCtx Nothing model

loaderHead :: LByteString
loaderHead =
  "<em style='font-size: 400%; border-bottom: 1px solid; margin-bottom: 4em; '>Union mounting notebook layers; please wait ...</em>"

renderLmlHtml :: Model -> MN.Note -> LByteString
renderLmlHtml model note = do
  let r = note ^. MN.noteRoute
      meta = Meta.getEffectiveRouteMetaWith (note ^. MN.noteMeta) r model
      tCtx = C.mkTemplateRenderCtx model r meta
      templateName = lookupTemplateName meta
      withLoadingMessage =
        if M.inLiveServer model && model ^. M.modelStatus == M.Status_Loading
          then (loaderHead <>)
          else id
  withLoadingMessage . C.renderModelTemplate model templateName $ do
    C.commonSplices (C.withLinkInlineCtx tCtx) model meta (note ^. MN.noteTitle)
    let backlinksSplice (bs :: [(R.LMLRoute, NonEmpty [B.Block])]) =
          Splices.listSplice bs "backlink" $
            \(source, contexts) -> do
              -- TODO: reuse note splice
              "backlink:note:title" ## C.titleSplice tCtx (M.modelLookupTitle source model)
              "backlink:note:url" ## HI.textSplice (SR.siteRouteUrl model $ SR.lmlSiteRoute source)
              "backlink:note:contexts" ## Splices.listSplice (toList contexts) "context" $ \backlinkCtx -> do
                let ctxDoc :: Pandoc = Pandoc mempty $ one $ B.Div B.nullAttr backlinkCtx
                "context:body" ## C.withInlineCtx tCtx $ \ctx ->
                  Splices.pandocSplice ctx ctxDoc
    -- Sidebar navigation
    routeTreeSplice tCtx (Just r) model
    "ema:breadcrumbs"
      ## C.routeBreadcrumbs tCtx model r
    -- Note stuff
    "ema:note:title"
      ## C.titleSplice tCtx (note ^. MN.noteTitle)
    let modelRoute = R.liftModelRoute . R.lmlRouteCase $ r
    "ema:note:backlinks"
      ## backlinksSplice (G.modelLookupBacklinks modelRoute model)
    let (backlinksDaily, backlinksNoDaily) = partition (Calendar.isDailyNote . fst) $ G.modelLookupBacklinks modelRoute model
    "ema:note:backlinks:daily"
      ## backlinksSplice backlinksDaily
    "ema:note:backlinks:nodaily"
      ## backlinksSplice backlinksNoDaily
    let folgeAnc = G.modelFolgezettelAncestorTree modelRoute model
    "ema:note:uptree"
      ## Splices.treeSplice (const ()) folgeAnc
      $ \(last -> nodeRoute) children -> do
        "node:text" ## C.titleSplice tCtx $ M.modelLookupTitle nodeRoute model
        "node:url" ## HI.textSplice $ SR.siteRouteUrl model $ SR.lmlSiteRoute nodeRoute
        "tree:open" ## Heist.ifElseISplice (not . null $ children)
    "ema:note:uptree:nonempty" ## Heist.ifElseISplice (not . null $ folgeAnc)
    "ema:note:uptreeStr" ## HI.textSplice (toText . Shower.shower $ folgeAnc)
    "ema:note:pandoc"
      ## C.withBlockCtx tCtx
      $ \ctx ->
        Splices.pandocSplice ctx (prepareNoteDoc model $ MN._noteDoc note)

-- | If there is no 'current route', all sub-trees are marked as active/open.
routeTreeSplice ::
  Monad n =>
  C.TemplateRenderCtx n ->
  Maybe R.LMLRoute ->
  Model ->
  H.Splices (HI.Splice n)
routeTreeSplice tCtx mr model = do
  "ema:route-tree"
    ## ( let tree = PathTree.treeDeleteChild "index" $ model ^. M.modelNav
             getOrder tr =
               ( Meta.lookupRouteMeta @Int 0 (one "order") tr model,
                 maybe (Tit.fromRoute tr) MN._noteTitle $ M.modelLookupNoteByRoute tr model
               )
             getCollapsed tr =
               Meta.lookupRouteMeta @Bool True ("template" :| ["sidebar", "collapsed"]) tr model
             mkLmlRoute = R.liftLMLRoute . R.R @('LMLType 'Md)
             lmlRouteSlugs = R.unRoute . R.lmlRouteCase
          in Splices.treeSplice (getOrder . mkLmlRoute) tree $ \(mkLmlRoute -> nodeRoute) children -> do
               "node:text" ## C.titleSplice tCtx $ M.modelLookupTitle nodeRoute model
               "node:url" ## HI.textSplice $ SR.siteRouteUrl model $ SR.lmlSiteRoute nodeRoute
               let isActiveNode = Just nodeRoute == mr
                   isActiveTree =
                     -- Active tree checking is applicable only when there is an
                     -- active route (i.e., mr is a Just)
                     flip (maybe True) mr $ \r ->
                       toList (lmlRouteSlugs nodeRoute) `NE.isPrefixOf` lmlRouteSlugs r
                   openTree =
                     isActiveTree -- Active tree is always open
                       || not (getCollapsed nodeRoute)
               "node:active" ## Heist.ifElseISplice isActiveNode
               "node:terminal" ## Heist.ifElseISplice (null children)
               "tree:childrenCount" ## HI.textSplice (show $ length children)
               "tree:open" ## Heist.ifElseISplice openTree
       )

lookupTemplateName :: ConvertUtf8 Text b => Aeson.Value -> b
lookupTemplateName meta =
  encodeUtf8 $ MN.lookupAeson @Text defaultTemplate ("template" :| ["name"]) meta
  where
    defaultTemplate = "templates/layouts/book"

withTemplateName :: Text -> Aeson.Value
withTemplateName =
  MN.oneAesonText (toList $ "template" :| ["name"])

withSiteTitle :: Text -> Aeson.Value
withSiteTitle =
  MN.oneAesonText (toList $ "page" :| ["siteTitle"])
