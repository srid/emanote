module Emanote.View.Template (emanoteSiteOutput, render) where

import Control.Monad.Logger (MonadLoggerIO)
import Data.Aeson.Types qualified as Aeson
import Data.List (partition)
import Data.List.NonEmpty qualified as NE
import Data.Map.Syntax ((##))
import Data.Text qualified as T
import Data.Tree.Path qualified as PathTree
import Ema qualified
import Emanote.Model (Model, ModelEma)
import Emanote.Model qualified as M
import Emanote.Model.Calendar qualified as Calendar
import Emanote.Model.Graph qualified as G
import Emanote.Model.Meta qualified as Meta
import Emanote.Model.Note qualified as MN
import Emanote.Model.SData qualified as SData
import Emanote.Model.Stork (renderStorkIndex)
import Emanote.Route qualified as R
import Emanote.Route.SiteRoute (SiteRoute)
import Emanote.Route.SiteRoute qualified as SR
import Emanote.Route.SiteRoute.Class (indexRoute)
import Emanote.View.Common qualified as C
import Emanote.View.Export (renderGraphExport)
import Emanote.View.Feed (feedDiscoveryLink, renderFeed)
import Emanote.View.TagIndex qualified as TagIndex
import Emanote.View.TaskIndex qualified as TaskIndex
import Heist qualified as H
import Heist.Extra.Splices.List qualified as Splices
import Heist.Extra.Splices.Pandoc qualified as Splices
import Heist.Extra.Splices.Pandoc.Ctx (emptyRenderCtx)
import Heist.Extra.Splices.Tree qualified as Splices
import Heist.Interpreted qualified as HI
import Heist.Splices qualified as Heist
import Optics.Core (Prism', review)
import Optics.Operators ((.~), (^.))
import Relude
import Text.Blaze.Renderer.XmlHtml qualified as RX
import Text.Pandoc.Builder qualified as B
import Text.Pandoc.Definition (Pandoc (..))

emanoteSiteOutput :: (MonadIO m, MonadLoggerIO m) => Prism' FilePath SiteRoute -> ModelEma -> SR.SiteRoute -> m (Ema.Asset LByteString)
emanoteSiteOutput rp model' r = do
  let model = M.withRoutePrism rp model'
  render model r <&> fmap fixStaticUrl
  where
    -- See the FIXME in more-head.tpl.
    fixStaticUrl :: LByteString -> LByteString
    fixStaticUrl s =
      case findPrefix of
        Nothing -> s
        Just prefix ->
          -- Patch the URL in CSS's "src" attribute.
          encodeUtf8
            . T.replace "src: url(_emanote-static/" ("src: url(" <> prefix <> "_emanote-static/")
            . decodeUtf8
            $ s
      where
        -- Find the "prefix" in PrefixedRoute if Emanote is used as a library.
        findPrefix :: Maybe Text
        findPrefix = do
          let indexR = toText $ review rp indexRoute
          prefix <- T.stripSuffix "-/all.html" indexR
          guard $ not $ T.null prefix
          pure prefix

render :: (MonadIO m, MonadLoggerIO m) => Model -> SR.SiteRoute -> m (Ema.Asset LByteString)
render m sr =
  let setErrorPageMeta =
        MN.noteMeta .~ SData.mergeAesons (withTemplateName "/templates/error" :| [withSiteTitle "Emanote Error"])
   in case sr of
        SR.SiteRoute_MissingR urlPath -> do
          let hereRoute = R.decodeHtmlRoute urlPath
              note404 =
                MN.missingNote hereRoute (toText urlPath)
                  & setErrorPageMeta
                  & MN.noteTitle .~ "! Missing link"
          pure $ Ema.AssetGenerated Ema.Html $ renderLmlHtml m note404
        SR.SiteRoute_AmbiguousR urlPath notes -> do
          let noteAmb =
                MN.ambiguousNoteURL urlPath notes
                  & setErrorPageMeta
                  & MN.noteTitle .~ "! Ambiguous link"
          pure $ Ema.AssetGenerated Ema.Html $ renderLmlHtml m noteAmb
        SR.SiteRoute_ResourceRoute r -> pure $ renderResourceRoute m r
        SR.SiteRoute_VirtualRoute r -> renderVirtualRoute m r

renderResourceRoute :: Model -> SR.ResourceRoute -> Ema.Asset LByteString
renderResourceRoute m = \case
  SR.ResourceRoute_LML SR.LMLView_Html r -> do
    case M.modelLookupNoteByRoute r m of
      Just note ->
        Ema.AssetGenerated Ema.Html $ renderLmlHtml m note
      Nothing ->
        -- This should never be reached because decodeRoute looks up the model.
        error $ "Bad route: " <> show r
  SR.ResourceRoute_LML SR.LMLView_Atom r ->
    case M.modelLookupNoteByRoute r m of
      Just note -> case renderFeed m note of
        Left err -> error $ toStrict $ "Bad feed: " <> show r <> ": " <> err
        Right feed -> Ema.AssetGenerated Ema.Other feed
      Nothing ->
        -- This should never be reached because decodeRoute looks up the model.
        error $ "Bad route: " <> show r
  SR.ResourceRoute_StaticFile _ fpAbs ->
    Ema.AssetStatic fpAbs

renderVirtualRoute :: (MonadIO m, MonadLoggerIO m) => Model -> SR.VirtualRoute -> m (Ema.Asset LByteString)
renderVirtualRoute m = \case
  SR.VirtualRoute_TagIndex mtag ->
    pure $ Ema.AssetGenerated Ema.Html $ TagIndex.renderTagIndex m mtag
  SR.VirtualRoute_Index ->
    pure $ Ema.AssetGenerated Ema.Html $ renderSRIndex m
  SR.VirtualRoute_Export ->
    pure $ Ema.AssetGenerated Ema.Other $ renderGraphExport m
  SR.VirtualRoute_StorkIndex ->
    Ema.AssetGenerated Ema.Other <$> renderStorkIndex m
  SR.VirtualRoute_TaskIndex ->
    pure $ Ema.AssetGenerated Ema.Html $ TaskIndex.renderTasks m

renderSRIndex :: Model -> LByteString
renderSRIndex model = do
  let (r, meta) = C.defaultRouteMeta model
      tCtx = C.mkTemplateRenderCtx model r meta
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
      ctx = C.mkTemplateRenderCtx model r meta
      templateName = lookupTemplateName meta
      -- Force a doctype into the generated HTML as a workaround for Heist
      -- discarding it. See: https://github.com/srid/emanote/issues/216
      withDoctype = ("<!DOCTYPE html>\n" <>)
      withLoadingMessage =
        if M.inLiveServer model && model ^. M.modelStatus == M.Status_Loading
          then (loaderHead <>)
          else id
  withDoctype . withLoadingMessage . C.renderModelTemplate model templateName $ do
    C.commonSplices (C.withLinkInlineCtx ctx) model meta (note ^. MN.noteTitle)
    let backlinksSplice (bs :: [(R.LMLRoute, NonEmpty [B.Block])]) =
          Splices.listSplice bs "backlink" $
            \(source, contexts) -> do
              let bnote = fromMaybe (error "backlink note missing - impossible") $ M.modelLookupNoteByRoute source model
                  bmeta = Meta.getEffectiveRouteMetaWith (bnote ^. MN.noteMeta) source model
                  bctx = C.mkTemplateRenderCtx model source bmeta
              -- TODO: reuse note splice
              "backlink:note:title" ## C.titleSplice bctx (M.modelLookupTitle source model)
              "backlink:note:url" ## HI.textSplice (SR.siteRouteUrl model $ SR.lmlSiteRoute source)
              "backlink:note:contexts" ## Splices.listSplice (toList contexts) "context" $ \backlinkCtx -> do
                let ctxDoc = Pandoc mempty $ one $ B.Div B.nullAttr backlinkCtx
                "context:body" ## C.withInlineCtx bctx $ \ctx' ->
                  Splices.pandocSplice ctx' ctxDoc
    -- Sidebar navigation
    routeTreeSplice ctx (Just r) model
    "ema:breadcrumbs" ##
      C.routeBreadcrumbs ctx model r
    -- Note stuff
    "ema:note:title" ##
      C.titleSplice ctx (note ^. MN.noteTitle)
    let modelRoute = R.ModelRoute_LML r
    "ema:note:source-path" ##
      HI.textSplice (toText . R.withLmlRoute R.encodeRoute $ r)
    "ema:note:url" ##
      HI.textSplice (SR.siteRouteUrl model . SR.lmlSiteRoute $ r)
    "emaNoteFeedUrl" ##
      pure . RX.renderHtmlNodes $
        if MN.noteHasFeed note
          then feedDiscoveryLink model note
          else mempty
    "ema:note:backlinks" ##
      backlinksSplice (G.modelLookupBacklinks modelRoute model)
    let (backlinksDaily, backlinksNoDaily) = partition (Calendar.isDailyNote . fst) $ G.modelLookupBacklinks modelRoute model
    "ema:note:backlinks:daily" ##
      backlinksSplice backlinksDaily
    "ema:note:backlinks:nodaily" ##
      backlinksSplice backlinksNoDaily
    let folgeAnc = G.modelFolgezettelAncestorTree modelRoute model
    "ema:note:uptree" ##
      Splices.treeSplice (const ()) folgeAnc $
        \(last -> nodeRoute) children -> do
          "node:text" ## C.titleSplice ctx $ M.modelLookupTitle nodeRoute model
          "node:url" ## HI.textSplice $ SR.siteRouteUrl model $ SR.lmlSiteRoute nodeRoute
          "tree:open" ## Heist.ifElseISplice (not . null $ children)
    "ema:note:uptree:nonempty" ## Heist.ifElseISplice (not . null $ folgeAnc)
    "ema:note:pandoc" ##
      C.withBlockCtx ctx $
        \ctx' ->
          Splices.pandocSplice ctx' (note ^. MN.noteDoc)

-- | If there is no 'current route', all sub-trees are marked as active/open.
routeTreeSplice ::
  Monad n =>
  C.TemplateRenderCtx n ->
  Maybe R.LMLRoute ->
  Model ->
  H.Splices (HI.Splice Identity)
routeTreeSplice tCtx mr model = do
  "ema:route-tree" ##
    ( let tree = PathTree.treeDeleteChild "index" $ model ^. M.modelNav
          getOrder tr =
            ( Meta.lookupRouteMeta @Int 0 (one "order") tr model
            , tr
            )
          getCollapsed tr =
            Meta.lookupRouteMeta @Bool True ("template" :| ["sidebar", "collapsed"]) tr model
          mkLmlRoute =
            M.resolveLmlRoute model . R.mkRouteFromSlugs
          lmlRouteSlugs = R.withLmlRoute R.unRoute
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
  encodeUtf8 $ SData.lookupAeson @Text defaultTemplate ("template" :| ["name"]) meta
  where
    defaultTemplate = "templates/layouts/book"

withTemplateName :: Text -> Aeson.Value
withTemplateName =
  SData.oneAesonText (toList $ "template" :| ["name"])

withSiteTitle :: Text -> Aeson.Value
withSiteTitle =
  SData.oneAesonText (toList $ "page" :| ["siteTitle"])
