{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- TODO: Split this sensibly.
module Emanote.View.Template (render) where

import Control.Lens.Operators ((^.))
import Control.Monad.Except (throwError)
import qualified Data.Aeson.Types as Aeson
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Map.Syntax ((##))
import qualified Data.Map.Syntax as MapSyntax
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import qualified Ema
import qualified Ema.CLI
import Ema.Helper.Markdown (plainify)
import qualified Ema.Helper.PathTree as PathTree
import qualified Ema.Helper.Tailwind as Tailwind
import Emanote.Model (Model)
import qualified Emanote.Model as M
import qualified Emanote.Model.Meta as Meta
import qualified Emanote.Model.Note as MN
import qualified Emanote.Model.Query as Q
import qualified Emanote.Model.Link.Rel as Rel
import qualified Emanote.Model.StaticFile as SF
import qualified Emanote.Prelude as EP
import Emanote.Route (FileType (LMLType), LML (Md))
import qualified Emanote.Route as R
import Emanote.View.SiteRoute (SiteRoute (..))
import qualified Emanote.View.SiteRoute as SR
import qualified Heist as H
import qualified Heist.Extra.Splices.List as Splices
import Heist.Extra.Splices.Pandoc (RenderCtx (..))
import qualified Heist.Extra.Splices.Pandoc as Splices
import qualified Heist.Extra.Splices.Tree as Splices
import qualified Heist.Extra.TemplateState as Tmpl
import qualified Heist.Interpreted as HI
import qualified Heist.Splices as Heist
import qualified Heist.Splices.Apply as HA
import qualified Heist.Splices.Bind as HB
import qualified Heist.Splices.Json as HJ
import qualified Text.Blaze.Renderer.XmlHtml as RX
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))
import qualified Text.XmlHtml as X

render :: Ema.CLI.Action -> Model -> SiteRoute -> Ema.Asset LByteString
render emaAction m = \case
  SRStaticFile (_r, fpAbs) ->
    Ema.AssetStatic fpAbs
  SRLMLFile lmlRoute -> do
    case M.modelLookupNoteByRoute lmlRoute m of
      Just note ->
        Ema.AssetGenerated Ema.Html $ renderLmlHtml emaAction m note
      Nothing ->
        error $ "Bad route: " <> show lmlRoute
  SRIndex ->
    Ema.AssetGenerated Ema.Html $ rendeSRIndex emaAction m

rendeSRIndex :: Ema.CLI.Action -> Model -> LByteString
rendeSRIndex emaAction model = do
  let meta = Meta.getIndexYamlMeta model
  flip (Tmpl.renderHeistTemplate "templates/special/index") (model ^. M.modelHeistTemplate) $ do
    commonSplices emaAction meta "@Index"
    routeTreeSplice Nothing model

renderLmlHtml :: Ema.CLI.Action -> Model -> MN.Note -> LByteString
renderLmlHtml emaAction model note = do
  let r = note ^. MN.noteRoute
      meta = Meta.getEffectiveRouteMeta r model
      templateName = MN.lookupAeson @Text "templates/layouts/book" ("template" :| ["name"]) meta
      rewriteClass = MN.lookupAeson @(Map Text Text) mempty ("pandoc" :| ["rewriteClass"]) meta
      pageTitle = M.modelLookupTitle r model
  flip (Tmpl.renderHeistTemplate templateName) (model ^. M.modelHeistTemplate) $ do
    commonSplices emaAction meta pageTitle
    -- Sidebar navigation
    routeTreeSplice (Just r) model
    "ema:breadcrumbs"
      ## Splices.listSplice (init $ R.routeInits . R.linkableLMLRouteCase $ r) "each-crumb"
      $ \(R.liftLinkableLMLRoute -> crumbR) ->
        MapSyntax.mapV HI.textSplice $ do
          "crumb:url" ## Ema.routeUrl model $ SR.SRLMLFile crumbR
          "crumb:title" ## M.modelLookupTitle crumbR model
    -- Note stuff
    "ema:note:title"
      ## HI.textSplice pageTitle
    "ema:note:backlinks"
      ## Splices.listSplice (M.modelLookupBacklinks (R.liftLinkableRoute . R.linkableLMLRouteCase $ r) model) "backlink"
      $ \(source, ctx) -> do
        let ctxDoc :: Pandoc = Pandoc mempty $ one $ B.Div B.nullAttr ctx
        -- TODO: reuse note splice
        "backlink:note:title" ## HI.textSplice (M.modelLookupTitle source model)
        "backlink:note:url" ## HI.textSplice (Ema.routeUrl model $ SR.SRLMLFile source)
        "backlink:note:context"
          ## Splices.pandocSpliceWithCustomClass
            rewriteClass
            (const . const $ Nothing)
            (const . const $ Nothing)
          $ ctxDoc & resolvePandoc
    "ema:note:pandoc"
      ## Splices.pandocSpliceWithCustomClass
        rewriteClass
        (querySplice model)
        (const . const $ Nothing)
      $ note ^. MN.noteDoc
        & ( EP.withoutH1 -- Because, handling note title separately
              >>> resolvePandoc
          )
  where
    resolvePandoc =
      EP.rewriteLinks (resolveUrl emaAction model)

commonSplices :: Monad n => Ema.CLI.Action -> Aeson.Value -> Text -> H.Splices (HI.Splice n)
commonSplices emaAction meta routeTitle = do
  let siteTitle = MN.lookupAeson @Text "Emabook Site" ("page" :| ["siteTitle"]) meta
  -- Heist helpers
  "bind" ## HB.bindImpl
  "apply" ## HA.applyImpl
  -- Add tailwind css shim
  "tailwindCssShim"
    ## pure (RX.renderHtmlNodes $ Tailwind.twindShim emaAction)
  "ema:metadata"
    ## HJ.bindJson meta
  "ema:title" ## HI.textSplice routeTitle
  "ema:titleFull"
    ## HI.textSplice
    $ if routeTitle == siteTitle
      then siteTitle
      else routeTitle <> " – " <> siteTitle

-- | If there is no 'current route', all sub-trees are marked as active/open.
routeTreeSplice :: Monad n => Maybe R.LinkableLMLRoute -> Model -> H.Splices (HI.Splice n)
routeTreeSplice mr model = do
  "ema:route-tree"
    ## ( let tree = PathTree.treeDeleteChild "index" $ model ^. M.modelNav
             getOrder tr =
               ( Meta.lookupMeta @Int 0 (one "order") tr model,
                 maybe (R.routeBaseName . R.linkableLMLRouteCase $ tr) MN.noteTitle $ M.modelLookupNoteByRoute tr model
               )
             getCollapsed tr =
               Meta.lookupMeta @Bool True ("template" :| ["sidebar", "collapsed"]) tr model
             mkLmlRoute = R.liftLinkableLMLRoute . R.R @('LMLType 'Md)
             lmlRouteSlugs = R.unRoute . R.linkableLMLRouteCase
          in Splices.treeSplice (getOrder . mkLmlRoute) tree $ \(mkLmlRoute -> nodeRoute) children -> do
               "node:text" ## HI.textSplice $ M.modelLookupTitle nodeRoute model
               "node:url" ## HI.textSplice $ Ema.routeUrl model $ SR.SRLMLFile nodeRoute
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

querySplice :: Monad n => Model -> RenderCtx n -> B.Block -> Maybe (HI.Splice n)
querySplice model RenderCtx {..} blk = do
  B.CodeBlock
    (_id', classes, _attrs)
    (Q.parseQuery -> Just q) <-
    pure blk
  guard $ List.elem "query" classes
  let mOtherCls = nonEmpty (List.delete "query" classes) <&> T.intercalate " " . toList
  -- TODO: This tag still remains in the HTML; it should be removed.
  queryNode <- childElementTagWithClass "CodeBlock:Query" mOtherCls rootNode
  let splices = do
        "query"
          ## HI.textSplice (show q)
        "result"
          ## (HI.runChildrenWith . noteSplice model) `foldMapM` Q.runQuery model q
  pure $
    H.localHS (HI.bindSplices splices) $
      HI.runNode queryNode
  where
    childElementTagWithClass tag mCls node = do
      queryNodes <- nonEmpty $ X.childElementsTag tag node
      fmap head . nonEmpty $
        NE.filter ((== mCls) . X.getAttribute "class") queryNodes

-- TODO: Reuse this elsewhere
noteSplice :: Monad n => Model -> MN.Note -> H.Splices (HI.Splice n)
noteSplice model note = do
  "note:title" ## HI.textSplice (MN.noteTitle note)
  "note:url" ## HI.textSplice (Ema.routeUrl model $ SR.SRLMLFile $ note ^. MN.noteRoute)
  "note:metadata" ## HJ.bindJson (note ^. MN.noteMeta)

resolveUrl :: Ema.CLI.Action -> Model -> [(Text, Text)] -> ([B.Inline], Text) -> Either Text ([B.Inline], Text)
resolveUrl emaAction model linkAttrs x@(inner, url) =
  fromMaybe (Right x) $ do
    fmap (fmap renderUrl)
      . resolveUnresolvedRelTarget model
      <=< Rel.parseUnresolvedRelTarget linkAttrs
      $ url
  where
    renderUrl ::
      (SiteRoute, Maybe UTCTime) ->
      ([B.Inline], Text)
    renderUrl (r, mTime) =
      let isWikiLinkSansCustom = url == plainify inner
          mQuery = do
            -- In live server mode, append last modification time if any, such
            -- that the browser is forced to refresh the inline image on hot
            -- reload (Ema's DOM patch).
            guard $ emaAction == Ema.CLI.Run
            t <- mTime
            pure $ toText $ "?t=" <> formatTime defaultTimeLocale "%s" t
       in ( fromMaybe
              inner
              ( guard isWikiLinkSansCustom >> wikiLinkDefaultInnerText r
              ),
            Ema.routeUrl model r <> fromMaybe "" mQuery
          )
      where
        wikiLinkDefaultInnerText = \case
          SRLMLFile lmlR -> do
            one . B.Str . MN.noteTitle <$> M.modelLookupNoteByRoute lmlR model
          SRStaticFile _ -> do
            -- Just append a file: prefix.
            pure $ B.Str "File: " : inner
          SRIndex ->
            Nothing

resolveUnresolvedRelTarget ::
  Model -> Rel.UnresolvedRelTarget -> Maybe (Either Text (SiteRoute, Maybe UTCTime))
resolveUnresolvedRelTarget model = \case
  Right r ->
    pure <$> resolveLinkableRoute r
  Left wl ->
    case nonEmpty (M.modelResolveWikiLink wl model) of
      Nothing -> do
        pure $ throwError "Unresolved wiki-link"
      Just targets ->
        -- TODO: Deal with ambiguous targets here
        pure <$> resolveLinkableRoute (head targets)
  where
    resolveLinkableRoute r =
      case R.linkableRouteCase r of
        Left lmlR -> do
          lmlRoute lmlR model
            <&> (,Nothing)
        Right sR -> do
          sf <- M.modelLookupStaticFileByRoute sR model
          pure $
            SR.staticFileSiteRoute sf
              & (,Just $ sf ^. SF.staticFileTime)
    lmlRoute :: R.LinkableLMLRoute -> Model -> Maybe SiteRoute
    lmlRoute r m = do
      SR.noteFileSiteRoute <$> M.modelLookupNoteByRoute r m
