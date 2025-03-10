module Emanote.View.Template (emanoteSiteOutput, render) where

import Commonmark.Extensions.WikiLink qualified as WikiLink
import Control.Monad.Logger (MonadLoggerIO)
import Data.Aeson.Optics (key, _String)
import Data.Aeson.Types qualified as Aeson
import Data.List (partition)
import Data.Map.Syntax ((##))
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Tree qualified as Tree
import Ema qualified
import Emanote.Model (Model, ModelEma)
import Emanote.Model qualified as M
import Emanote.Model.Calendar qualified as Calendar
import Emanote.Model.Graph qualified as G
import Emanote.Model.Meta qualified as Meta
import Emanote.Model.Note qualified as MN
import Emanote.Model.SData qualified as SData
import Emanote.Model.Stork (renderStorkIndex)
import Emanote.Model.Toc (newToc, renderToc, tocUnnecessaryToRender)
import Emanote.Route qualified as R
import Emanote.Route.SiteRoute (SiteRoute)
import Emanote.Route.SiteRoute qualified as SR
import Emanote.Route.SiteRoute.Class (indexRoute)
import Emanote.View.Common qualified as C
import Emanote.View.Export (renderJSONExport)
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
import Network.URI.Slug qualified as Slug
import Optics.Core (Prism', review)
import Optics.Operators ((.~), (^.), (^?))
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
render m = \case
  SR.SiteRoute_MissingR urlPath -> do
    let hereRoute = R.decodeHtmlRoute urlPath
        note404 =
          MN.missingNote hereRoute (toText urlPath)
            & setErrorPageMeta
            & MN.noteTitle
            .~ "! Missing link"
    pure $ Ema.AssetGenerated Ema.Html $ renderLmlHtml m note404
  SR.SiteRoute_AmbiguousR urlPath notes -> do
    let noteAmb =
          MN.ambiguousNoteURL urlPath notes
            & setErrorPageMeta
            & MN.noteTitle
            .~ "! Ambiguous link"
    pure $ Ema.AssetGenerated Ema.Html $ renderLmlHtml m noteAmb
  SR.SiteRoute_ResourceRoute r -> pure $ renderResourceRoute m r
  SR.SiteRoute_VirtualRoute r -> renderVirtualRoute m r
  where
    setErrorPageMeta =
      MN.noteMeta .~ SData.mergeAesons (withTemplateName "/templates/error" :| [withSiteTitle "Emanote Error"])

renderResourceRoute :: Model -> SR.ResourceRoute -> Ema.Asset LByteString
renderResourceRoute m = \case
  SR.ResourceRoute_LML view r -> do
    case M.modelLookupNoteByRoute (view, r) m of
      Just (R.LMLView_Html, note) ->
        Ema.AssetGenerated Ema.Html $ renderLmlHtml m note
      Just (R.LMLView_Atom, note) ->
        case renderFeed m note of
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
    pure $ Ema.AssetGenerated Ema.Other $ renderJSONExport m
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
    routeTreeSplices tCtx Nothing model

loaderHead :: LByteString
loaderHead =
  "<em style='font-size: 400%; border-bottom: 1px solid; margin-bottom: 4em; '>Union mounting notebook layers; please wait ...</em>"

patchMeta :: Aeson.Value -> Aeson.Value
patchMeta meta =
  -- Convert relative to absolute URLs in "page.image", because some sites
  -- (Twitter) require "og:image" to be absolute.
  SData.modifyAeson
    ("page" :| ["image"])
    ( \case
        Just (Aeson.String v)
          | not (":" `T.isInfixOf` v) && siteUrl /= "" ->
              Just $ Aeson.String $ siteUrl <> "/" <> v
        x -> x
    )
    meta
  where
    siteUrl = SData.lookupAeson @Text "" ("page" :| ["siteUrl"]) meta

renderLmlHtml :: Model -> MN.Note -> LByteString
renderLmlHtml model note = do
  let r = note ^. MN.noteRoute
      meta = patchMeta $ Meta.getEffectiveRouteMetaWith (note ^. MN.noteMeta) r model
      toc = newToc $ note ^. MN.noteDoc
      sourcePath = fromMaybe (R.withLmlRoute R.encodeRoute r) $ do
        fmap snd $ note ^. MN.noteSource
      -- Force a doctype into the generated HTML as a workaround for Heist
      -- discarding it. See: https://github.com/srid/emanote/issues/216
      withDoctype = ("<!DOCTYPE html>\n" <>)
      withLoadingMessage =
        if M.inLiveServer model && model ^. M.modelStatus == M.Status_Loading
          then (loaderHead <>)
          else id
  withDoctype . withLoadingMessage . C.renderModelTemplate model (lookupTemplateName meta) $ do
    let ctx = C.mkTemplateRenderCtx model r meta
    C.commonSplices (C.withLinkInlineCtx ctx) model meta (note ^. MN.noteTitle)
    -- Template flags
    forM_ ["uptree", "breadcrumbs", "sidebar", "toc"] $ \flag -> do
      let hasFlag' = Meta.lookupRouteMeta @Bool False ("template" :| [flag, "enable"]) r model
          hasFlag = if flag == "toc" then hasFlag' && not (tocUnnecessaryToRender toc) else hasFlag'
      "ema:has:" <> flag ## Heist.ifElseISplice hasFlag
    -- Sidebar navigation
    routeTreeSplices ctx (Just r) model
    "ema:breadcrumbs" ##
      C.routeBreadcrumbs ctx model r
    -- Note stuff
    "ema:note:title" ##
      C.titleSplice ctx (note ^. MN.noteTitle)
    "ema:note:source-path" ##
      HI.textSplice
        $ toText sourcePath
    "ema:note:url" ##
      HI.textSplice (SR.siteRouteUrl model . SR.lmlSiteRoute $ (R.LMLView_Html, r))
    "emaNoteFeedUrl" ##
      pure
        . RX.renderHtmlNodes
        $ if MN.noteHasFeed note
          then feedDiscoveryLink model note
          else mempty

    let mNext = getMetaList note "next"
    forM_ mNext $ \nexts ->
      "ema:note:next" ## navSplice model note nexts
    "ema:has:next" ## Heist.ifElseISplice (isJust mNext)
    let mPrev = getMetaList note "prev"
    traceShowM (MN._noteMeta note)
    forM_ mPrev $ \prevs ->
      "ema:note:prev" ## navSplice model note prevs
    "ema:has:prev" ## Heist.ifElseISplice (isJust mPrev)

    "ema:note:backlinks" ##
      backlinksSplice model (G.modelLookupBacklinks r model)
    let (backlinksDaily, backlinksNoDaily) = partition (Calendar.isDailyNote . fst) $ G.modelLookupBacklinks r model
    "ema:note:backlinks:daily" ##
      backlinksSplice model backlinksDaily
    "ema:note:backlinks:nodaily" ##
      backlinksSplice model backlinksNoDaily
    let folgeAnc = G.modelFolgezettelAncestorTree model r
    "ema:note:uptree" ##
      Splices.treeSplice (\_ _ -> ()) folgeAnc
        $ \(last -> nodeRoute) children -> do
          "node:text" ## C.titleSplice ctx $ M.modelLookupTitle nodeRoute model
          "node:url" ## HI.textSplice $ SR.siteRouteUrl model $ SR.lmlSiteRoute (R.LMLView_Html, nodeRoute)
          "tree:open" ## Heist.ifElseISplice (not . null $ children)
    "ema:note:uptree:nonempty" ## Heist.ifElseISplice (not . null $ folgeAnc)
    "ema:note:pandoc" ##
      C.withBlockCtx ctx
        $ \ctx' ->
          Splices.pandocSplice ctx' (note ^. MN.noteDoc)
    "ema:note:toc" ##
      C.withBlockCtx ctx
        $ \ctx' ->
          renderToc ctx' toc

-- | Return a meta top attribute as a single elem or a list of elem
getMetaList :: MN.Note -> Aeson.Key -> Maybe [Text]
getMetaList note k =
  MN._noteMeta note
    ^? key k >>= \v ->
      let singleValue = (: []) <$> (v ^? _String)
          listValue = Aeson.parseMaybe Aeson.parseJSON v -- Q: how to do that with aeson-optics?
       in singleValue <|> listValue

navSplice :: Model -> MN.Note -> [Text] -> HI.Splice Identity
navSplice model note links = (HI.runChildrenWith . (navSplices model)) `foldMapM` routes
  where
    mkWikiLink = WikiLink.mkWikiLinkFromSlugs . pure . Slug.decodeSlug
    routes = mapMaybe (G.lookupNoteByWikiLink model (note ^. MN.noteRoute) . mkWikiLink) links

navSplices :: Model -> R.LMLRoute -> H.Splices (HI.Splice Identity)
navSplices model source = do
  "nav:title" ## C.titleSplice ctx (M.modelLookupTitle source model)
  "nav:url" ## HI.textSplice (SR.siteRouteUrl model $ SR.lmlSiteRoute (R.LMLView_Html, source))
  where
    note = fromMaybe (error "backlink note missing - impossible") $ M.modelLookupNoteByRoute' source model
    meta = Meta.getEffectiveRouteMetaWith (note ^. MN.noteMeta) source model
    ctx = C.mkTemplateRenderCtx model source meta

backlinksSplice :: Model -> [(R.LMLRoute, NonEmpty [B.Block])] -> HI.Splice Identity
backlinksSplice model (bs :: [(R.LMLRoute, NonEmpty [B.Block])]) =
  Splices.listSplice bs "backlink"
    $ \(source, contexts) -> do
      let bnote = fromMaybe (error "backlink note missing - impossible") $ M.modelLookupNoteByRoute' source model
          bmeta = Meta.getEffectiveRouteMetaWith (bnote ^. MN.noteMeta) source model
          bctx = C.mkTemplateRenderCtx model source bmeta
      -- TODO: reuse note splice
      "backlink:note:title" ## C.titleSplice bctx (M.modelLookupTitle source model)
      "backlink:note:url" ## HI.textSplice (SR.siteRouteUrl model $ SR.lmlSiteRoute (R.LMLView_Html, source))
      "backlink:note:contexts" ##
        Splices.listSplice (toList contexts) "context"
          $ \backlinkCtx -> do
            let ctxDoc = Pandoc mempty $ one $ B.Div B.nullAttr backlinkCtx
            "context:body" ##
              C.withInlineCtx bctx
                $ \ctx' ->
                  Splices.pandocSplice ctx' ctxDoc

{- | Heist splice for the sidebar tree.

If there is no 'current route', all sub-trees are marked as active/open.
-}
routeTreeSplices :: (Monad n) => C.TemplateRenderCtx n -> Maybe R.LMLRoute -> Model -> H.Splices (HI.Splice Identity)
routeTreeSplices tCtx mCurrentRoute model = do
  "ema:route-tree" ##
    Splices.treeSplice getOrder (model ^. M.modelFolgezettelTree)
      $ \(last -> nodeRoute) children -> do
        let shortTitle = Meta.lookupRouteMeta @(Maybe Text) Nothing ("short-title" :| []) nodeRoute model
        "node:text" ## maybe (C.titleSplice tCtx $ M.modelLookupTitle nodeRoute model) HI.textSplice shortTitle
        "node:url" ## HI.textSplice $ SR.siteRouteUrl model $ SR.lmlSiteRoute (R.LMLView_Html, nodeRoute)
        let isActiveNode = Just nodeRoute == mCurrentRoute
            isActiveTree =
              -- Active tree checking is applicable only when there is an
              -- active route (i.e., mr is a Just)
              flip (maybe True) mCurrentRoute $ \r ->
                -- FIXME: Performance! (exponential complexity)
                let folgeAnc = Set.fromList $ concatMap Tree.flatten $ G.modelFolgezettelAncestorTree model r
                    isFolgeAnc = Set.member nodeRoute folgeAnc
                 in r == nodeRoute || isFolgeAnc
            openTree =
              isActiveTree -- Active tree is always open
                || not (getCollapsed nodeRoute)
        "node:active" ## Heist.ifElseISplice isActiveNode
        "node:activeTree" ## Heist.ifElseISplice isActiveTree
        "node:terminal" ## Heist.ifElseISplice (null children)
        "tree:childrenCount" ## HI.textSplice (show $ length children)
        "tree:open" ## Heist.ifElseISplice openTree
        "has-current-route" ## Heist.ifElseISplice (isJust mCurrentRoute)
  where
    getFoldersFirst tr =
      Meta.lookupRouteMeta @Bool False ("template" :| ["sidebar", "folders-first"]) tr model
    getOrder path children =
      let tr = last path
          isLeaf = null children
          priority = if getFoldersFirst tr && isLeaf then 1 else 0 :: Int
       in ( priority
          , Meta.lookupRouteMeta @Int 0 (one "order") tr model
          , tr
          )
    getCollapsed tr =
      Meta.lookupRouteMeta @Bool True ("template" :| ["sidebar", "collapsed"]) tr model

lookupTemplateName :: (ConvertUtf8 Text b) => Aeson.Value -> b
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
