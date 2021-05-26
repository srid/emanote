{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.Template (render) where

import Control.Lens.Operators ((^.))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Map.Syntax ((##))
import qualified Data.Map.Syntax as MapSyntax
import qualified Data.Text as T
import qualified Ema
import qualified Ema.Helper.PathTree as PathTree
import Emanote.Class (EmanoteRoute (..), htmlRouteForLmlRoute)
import Emanote.Model (Model)
import qualified Emanote.Model as M
import qualified Emanote.Model.Meta as Meta
import qualified Emanote.Model.Note as MN
import qualified Emanote.Model.Rel as Rel
import qualified Emanote.PandocUtil as PandocUtil
import Emanote.Route (Route)
import qualified Emanote.Route as R
import Emanote.Route.Ext (FileType (Html, LMLType), LML (Md))
import qualified Emanote.Route.Ext as Ext
import qualified Emanote.Route.WikiLinkTarget as WL
import qualified Heist.Extra.Splices.List as Splices
import qualified Heist.Extra.Splices.Pandoc as Splices
import qualified Heist.Extra.Splices.Tree as Splices
import qualified Heist.Extra.TemplateState as T
import qualified Heist.Interpreted as HI
import qualified Heist.Splices as Heist
import qualified Heist.Splices.Apply as HA
import qualified Heist.Splices.Bind as HB
import qualified Heist.Splices.Json as HJ
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Renderer.XmlHtml as RX
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))

render :: H.Html -> Model -> EmanoteRoute -> Ema.Asset LByteString
render x m = \case
  EROtherFile (_r, fpAbs) ->
    Ema.AssetStatic fpAbs
  ERNoteHtml (mdRouteForHtmlRoute -> r) ->
    Ema.AssetGenerated Ema.Html $ renderHtml x m r
  where
    mdRouteForHtmlRoute :: Route 'Html -> Route ('LMLType 'Md)
    mdRouteForHtmlRoute = coerce

renderHtml :: H.Html -> Model -> Route ('LMLType 'Md) -> LByteString
renderHtml tailwindShim model r = do
  let meta = Meta.getEffectiveRouteMeta r model
      templateName = Meta.lookupMetaFrom @Text "templates/_default" ("template" :| ["name"]) meta
      rewriteClass = Meta.lookupMetaFrom @(Map Text Text) mempty ("pandoc" :| ["rewriteClass"]) meta
      siteTitle = Meta.lookupMetaFrom @Text "Emabook Site" ("page" :| ["siteTitle"]) meta
      pageTitle = M.modelLookupTitle r model
  flip (T.renderHeistTemplate templateName) (model ^. M.modelHeistTemplate) $ do
    -- Heist helpers
    "bind" ## HB.bindImpl
    "apply" ## HA.applyImpl
    -- Add tailwind css shim
    "tailwindCssShim" ## pure (RX.renderHtmlNodes tailwindShim)
    -- Bind route-associated metadata to <html> so that they remain in scope
    -- throughout.
    "html"
      ## HJ.bindJson meta
    -- Sidebar navigation
    "ema:route-tree"
      ## ( let tree = PathTree.treeDeleteChild "index" $ model ^. M.modelNav
               getOrder tr =
                 (Meta.lookupMeta @Int 0 (one "order") tr model, maybe (R.routeFileBase tr) MN.noteTitle $ M.modelLookup tr model)
               getCollapsed tr =
                 Meta.lookupMeta @Bool True ("template" :| ["sidebar", "collapsed"]) tr model
            in Splices.treeSplice (getOrder . R.Route) tree $ \(R.Route -> nodeRoute) children -> do
                 "node:text" ## HI.textSplice $ M.modelLookupTitle nodeRoute model
                 "node:url" ## HI.textSplice $ noteUrl nodeRoute
                 let isActiveNode = nodeRoute == r
                     isActiveTree =
                       toList (R.unRoute nodeRoute) `NE.isPrefixOf` R.unRoute r
                     openTree =
                       isActiveTree -- Active tree is always open
                         || not (getCollapsed nodeRoute)
                 "node:active" ## Heist.ifElseISplice isActiveNode
                 "node:terminal" ## Heist.ifElseISplice (null children)
                 "tree:childrenCount" ## HI.textSplice (show $ length children)
                 "tree:open" ## Heist.ifElseISplice openTree
         )
    "ema:breadcrumbs"
      ## Splices.listSplice (init $ R.routeInits r) "each-crumb"
      $ \crumb ->
        MapSyntax.mapV HI.textSplice $ do
          "crumb:url" ## noteUrl crumb
          "crumb:title" ## M.modelLookupTitle crumb model
    -- Note stuff
    "ema:note:title"
      ## HI.textSplice pageTitle
    "ema:note:titleFull"
      ## HI.textSplice (if pageTitle == siteTitle then pageTitle else pageTitle <> " – " <> siteTitle)
    "ema:note:backlinks"
      ## Splices.listSplice (M.modelLookupBacklinks r model) "backlink"
      $ \(source, ctx) -> do
        let ctxDoc :: Pandoc = Pandoc mempty $ B.Div B.nullAttr <$> toList ctx
        -- TODO: reuse note splice
        "backlink:note:title" ## HI.textSplice (M.modelLookupTitle source model)
        "backlink:note:url" ## HI.textSplice (Ema.routeUrl $ ERNoteHtml $ htmlRouteForLmlRoute source)
        "backlink:note:context"
          ## Splices.pandocSplice
          $ ctxDoc
    "ema:note:pandoc"
      ## Splices.pandocSpliceWithCustomClass rewriteClass
      $ case M.modelLookup r model of
        Nothing ->
          -- This route doesn't correspond to any Markdown file on disk. Could be one of the reasons,
          -- 1. Refers to a folder route (and no ${folder}.md exists)
          -- 2. A broken wiki-links
          -- In both cases, we take the lenient approach, and display an empty page (but with title).
          -- TODO: Display folder children if this is a folder note. It is hinted to in the sidebar too.
          Pandoc mempty $ one $ B.Plain $ one $ B.Str "No Markdown file exists for this route."
        Just note ->
          note ^. MN.noteDoc
            & ( PandocUtil.withoutH1 -- Because, handling note title separately
                  >>> PandocUtil.rewriteLinks (resolveUrl model)
              )

-- | Convert .md or wiki links to their proper route url.
--
-- Requires resolution from the `model` state. Late resolution, in other words.
resolveUrl :: Model -> Text -> Text
resolveUrl model url =
  fromMaybe url $ do
    guard $ not $ isStaticAssetUrl url
    r <-
      Rel.parseUrl url >>= \case
        Right r -> do
          pure r
        Left wl ->
          case nonEmpty (M.modelLookupRouteByWikiLink wl model) of
            Nothing -> do
              -- TODO: Set an attribute for broken links, so templates can style it accordingly
              let fakeRouteUnder404 = R.Route @('Ext.LMLType 'Ext.Md) $ one "404" <> WL.unWikiLinkText wl
              pure fakeRouteUnder404
            Just targets ->
              -- TODO: Deal with ambiguous targets here
              pure $ head targets
    pure $ noteUrl r
  where
    isStaticAssetUrl s =
      any (\asset -> toText (R.routeSourcePath asset) `T.isPrefixOf` s) $ Map.keys $ model ^. M.modelStaticFiles

noteUrl :: Route ('LMLType x) -> Text
noteUrl =
  Ema.routeUrl . ERNoteHtml . htmlRouteForLmlRoute
