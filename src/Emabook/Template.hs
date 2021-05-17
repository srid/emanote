{-# LANGUAGE TypeApplications #-}

module Emabook.Template where

import Control.Lens.Operators ((^.))
import qualified Data.List.NonEmpty as NE
import Data.Map.Syntax ((##))
import qualified Data.Map.Syntax as MapSyntax
import qualified Data.Text as T
import Ema (Ema)
import qualified Ema
import qualified Ema.Helper.PathTree as PathTree
import Emabook.Model (Model)
import qualified Emabook.Model as M
import qualified Emabook.Model.Meta as Meta
import qualified Emabook.Model.Note as MN
import qualified Emabook.Model.Rel as Rel
import qualified Emabook.PandocUtil as PandocUtil
import Emabook.Route (MarkdownRoute)
import qualified Emabook.Route as R
import qualified Emabook.Route.Ext as Ext
import qualified Emabook.Route.WikiLinkTarget as WL
import qualified Heist.Extra.Splices.List as Splices
import qualified Heist.Extra.Splices.Pandoc as Splices
import qualified Heist.Extra.Splices.Tree as Splices
import qualified Heist.Extra.TemplateState as T
import qualified Heist.Interpreted as HI
import qualified Heist.Splices as Heist
import qualified Heist.Splices.Apply as HA
import qualified Heist.Splices.Bind as HB
import qualified Heist.Splices.Json as HJ
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))

render :: Ema Model MarkdownRoute => Model -> MarkdownRoute -> LByteString
render model r = do
  let meta = Meta.getEffectiveRouteMeta r model
      templateName = Meta.lookupMetaFrom @Text "_default" ("template" :| ["name"]) meta
  flip (T.renderHeistTemplate templateName) (model ^. M.modelHeistTemplate) $ do
    -- Heist helpers
    "bind" ## HB.bindImpl
    "apply" ## HA.applyImpl
    -- Bind route-associated metadata to <html> so that they remain in scope
    -- throughout.
    "html"
      ## HJ.bindJson meta
    -- Sidebar navigation
    "ema:route-tree"
      ## ( let tree = PathTree.treeDeleteChild "index" $ model ^. M.modelNav
               getOrder tr =
                 (Meta.lookupMeta @Int 0 "order" tr model, maybe (R.routeFileBase tr) MN.noteTitle $ M.modelLookup tr model)
            in Splices.treeSplice (getOrder . R.Route) tree $ \(R.Route -> nodeRoute) -> do
                 "node:text" ## HI.textSplice $ M.modelLookupTitle nodeRoute model
                 "node:url" ## HI.textSplice $ Ema.routeUrl nodeRoute
                 let isActiveNode = nodeRoute == r
                     isActiveTree =
                       toList (R.unRoute nodeRoute) `NE.isPrefixOf` R.unRoute r
                 "node:active" ## Heist.ifElseISplice isActiveNode
                 "tree:active" ## Heist.ifElseISplice isActiveTree
         )
    "ema:breadcrumbs"
      ## Splices.listSplice (init $ R.routeInits r) "each-crumb"
      $ \crumb ->
        MapSyntax.mapV HI.textSplice $ do
          "crumb:url" ## Ema.routeUrl crumb
          "crumb:title" ## M.modelLookupTitle crumb model
    -- Note stuff
    "ema:note:title"
      ## HI.textSplice
      $ M.modelLookupTitle r model
    "ema:note:backlinks"
      ## Splices.listSplice (M.modelLookupBacklinks r model) "backlink"
      $ \(source, ctx) -> do
        let ctxDoc :: Pandoc = Pandoc mempty $ B.Div B.nullAttr <$> toList ctx
        -- TODO: reuse note splice
        "backlink:note:title" ## HI.textSplice (M.modelLookupTitle source model)
        "backlink:note:url" ## HI.textSplice (Ema.routeUrl source)
        "backlink:note:context"
          ## Splices.pandocSplice
          $ ctxDoc
    "ema:note:pandoc"
      ## Splices.pandocSplice
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
resolveUrl :: Ema Model MarkdownRoute => Model -> Text -> Text
resolveUrl model url =
  fromMaybe url $ do
    guard $ not $ isStaticAssetUrl url
    Rel.parseUrl url >>= \case
      Right r -> do
        pure $ Ema.routeUrl r
      Left wl ->
        case nonEmpty (M.modelLookupRouteByWikiLink wl model) of
          Nothing -> do
            -- TODO: Set an attribute for broken links, so templates can style it accordingly
            let fakeRouteUnder404 = R.Route @Ext.Md $ one "404" <> WL.unWikiLinkText wl
            pure $ Ema.routeUrl fakeRouteUnder404
          Just targets ->
            -- TODO: Deal with ambiguous targets here
            pure $ Ema.routeUrl $ head targets
  where
    isStaticAssetUrl s =
      any (\asset -> ("/" <> toText asset) `T.isPrefixOf` s) (Ema.staticAssets $ Proxy @MarkdownRoute)
