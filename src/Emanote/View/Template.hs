{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- TODO: Split this sensibly.
module Emanote.View.Template (render) where

import Control.Lens.Operators ((^.))
import Data.List (partition)
import qualified Data.List.NonEmpty as NE
import Data.Map.Syntax ((##))
import Data.WorldPeace.Union
  ( absurdUnion,
  )
import qualified Ema
import qualified Ema.CLI
import qualified Ema.Helper.PathTree as PathTree
import Emanote.Model (Model)
import qualified Emanote.Model as M
import qualified Emanote.Model.Calendar as Calendar
import qualified Emanote.Model.Graph as G
import qualified Emanote.Model.Meta as Meta
import qualified Emanote.Model.Note as MN
import qualified Emanote.Model.Title as Tit
import Emanote.Pandoc.BuiltinFilters (prepareNoteDoc, preparePandoc)
import Emanote.Prelude (h)
import Emanote.Route (FileType (LMLType), LML (Md))
import qualified Emanote.Route as R
import qualified Emanote.Route.SiteRoute as SR
import Emanote.View.Common (commonSplices, inlineRenderers, linkInlineRenderers, mkRendererFromMeta, noteRenderers)
import qualified Emanote.View.TagIndex as TagIndex
import qualified Heist as H
import qualified Heist.Extra.Splices.List as Splices
import qualified Heist.Extra.Splices.Pandoc as Splices
import Heist.Extra.Splices.Pandoc.Ctx (emptyRenderCtx)
import qualified Heist.Extra.Splices.Tree as Splices
import qualified Heist.Extra.TemplateState as Tmpl
import qualified Heist.Interpreted as HI
import qualified Heist.Splices as Heist
import qualified Shower
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))

render :: Ema.CLI.Action -> Model -> SR.SiteRoute -> Ema.Asset LByteString
render emaAction m =
  absurdUnion
    `h` ( \(SR.MissingR urlPath) -> do
            let route404 = R.liftLMLRoute @('LMLType 'Md) . coerce $ R.decodeHtmlRoute urlPath
                note404 =
                  MN.mkEmptyNoteWith route404 $
                    one $ B.Para [B.Str $ "No note found for '" <> toText urlPath <> "'"]
            Ema.AssetGenerated Ema.Html $ renderLmlHtml emaAction m note404
        )
    `h` renderResourceRoute emaAction m
    `h` renderVirtualRoute emaAction m

renderResourceRoute :: Ema.CLI.Action -> Model -> SR.ResourceRoute -> Ema.Asset LByteString
renderResourceRoute emaAction m =
  absurdUnion
    `h` ( \(r :: R.LMLRoute) -> do
            case M.modelLookupNoteByRoute r m of
              Just note ->
                Ema.AssetGenerated Ema.Html $ renderLmlHtml emaAction m note
              Nothing ->
                -- This should never be reached because decodeRoute looks up the model.
                error $ "Bad route: " <> show r
        )
    `h` ( \(_ :: R.StaticFileRoute, fpAbs :: FilePath) -> do
            Ema.AssetStatic fpAbs
        )

renderVirtualRoute :: Ema.CLI.Action -> Model -> SR.VirtualRoute -> Ema.Asset LByteString
renderVirtualRoute emaAction m =
  absurdUnion
    `h` ( \(SR.TagIndexR mtag) ->
            Ema.AssetGenerated Ema.Html $ TagIndex.renderTagIndex emaAction m mtag
        )
    `h` ( \SR.IndexR ->
            Ema.AssetGenerated Ema.Html $ renderSRIndex emaAction m
        )

renderSRIndex :: Ema.CLI.Action -> Model -> LByteString
renderSRIndex emaAction model = do
  let meta = Meta.getIndexYamlMeta model
      withNoteRenderer = mkRendererFromMeta emaAction model meta
      withInlineCtx =
        withNoteRenderer linkInlineRenderers () ()
  flip (Tmpl.renderHeistTemplate "templates/special/index") (model ^. M.modelHeistTemplate) $ do
    commonSplices ($ emptyRenderCtx) emaAction model meta "Index"
    routeTreeSplice withInlineCtx Nothing model

renderLmlHtml :: Ema.CLI.Action -> Model -> MN.Note -> LByteString
renderLmlHtml emaAction model note = do
  let r = note ^. MN.noteRoute
      meta = Meta.getEffectiveRouteMeta r model
      withNoteRenderer = mkRendererFromMeta emaAction model meta
      withInlineCtx =
        withNoteRenderer inlineRenderers () ()
      withLinkInlineCtx =
        withNoteRenderer linkInlineRenderers () ()
      withBlockCtx =
        withNoteRenderer noteRenderers () r
      templateName = MN.lookupAeson @Text "templates/layouts/book" ("template" :| ["name"]) meta
      pageTitle = M.modelLookupTitle r model
  flip (Tmpl.renderHeistTemplate templateName) (model ^. M.modelHeistTemplate) $ do
    commonSplices withLinkInlineCtx emaAction model meta pageTitle
    -- TODO: We should be using withInlineCtx, so as to make the wikilink render in note title.
    let titleSplice titleDoc = withLinkInlineCtx $ \x ->
          Tit.titleSplice x (preparePandoc model) titleDoc
        backlinksSplice bs =
          Splices.listSplice bs "backlink" $
            \(source, backlinkCtx) -> do
              -- TODO: reuse note splice
              "backlink:note:title" ## titleSplice (M.modelLookupTitle source model)
              "backlink:note:url" ## HI.textSplice (SR.siteRouteUrl model $ SR.lmlSiteRoute source)
              "backlink:note:context"
                ## do
                  let ctxDoc :: Pandoc = Pandoc mempty $ one $ B.Div B.nullAttr backlinkCtx
                  withInlineCtx $ \ctx ->
                    Splices.pandocSplice ctx ctxDoc
    -- Sidebar navigation
    routeTreeSplice withLinkInlineCtx (Just r) model
    "ema:breadcrumbs"
      ## Splices.listSplice (init $ R.routeInits . R.lmlRouteCase $ r) "each-crumb"
      $ \(R.liftLMLRoute -> crumbR) -> do
        "crumb:url" ## HI.textSplice (SR.siteRouteUrl model $ SR.lmlSiteRoute crumbR)
        "crumb:title" ## titleSplice (M.modelLookupTitle crumbR model)
    -- Note stuff
    "ema:note:title"
      ## titleSplice pageTitle
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
        "node:text" ## withInlineCtx $ \ctx ->
          Tit.titleSplice ctx (preparePandoc model) $ M.modelLookupTitle nodeRoute model
        "node:url" ## HI.textSplice $ SR.siteRouteUrl model $ SR.lmlSiteRoute nodeRoute
        "tree:open" ## Heist.ifElseISplice (not . null $ children)
    "ema:note:uptree:nonempty" ## Heist.ifElseISplice (not . null $ folgeAnc)
    "ema:note:uptreeStr" ## HI.textSplice (toText . Shower.shower $ folgeAnc)
    "ema:note:pandoc"
      ## withBlockCtx
      $ \ctx ->
        Splices.pandocSplice ctx (prepareNoteDoc model $ MN._noteDoc note)

-- | If there is no 'current route', all sub-trees are marked as active/open.
routeTreeSplice ::
  Monad n =>
  ((Splices.RenderCtx n -> HI.Splice n) -> HI.Splice n) ->
  Maybe R.LMLRoute ->
  Model ->
  H.Splices (HI.Splice n)
routeTreeSplice withCtx mr model = do
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
               "node:text" ## withCtx $ \ctx ->
                 Tit.titleSplice ctx (preparePandoc model) $ M.modelLookupTitle nodeRoute model
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
