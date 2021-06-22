{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- TODO: Split this sensibly.
module Emanote.View.Template (render) where

import Control.Lens.Operators ((^.))
import qualified Data.Aeson.Types as Aeson
import qualified Data.List.NonEmpty as NE
import Data.Map.Syntax ((##))
import qualified Data.Map.Syntax as MapSyntax
import Data.Version (showVersion)
import Data.WorldPeace.Union
  ( absurdUnion,
  )
import qualified Ema
import qualified Ema.CLI
import qualified Ema.Helper.PathTree as PathTree
import qualified Ema.Helper.Tailwind as Tailwind
import Emanote.Model (Model)
import qualified Emanote.Model as M
import qualified Emanote.Model.Meta as Meta
import qualified Emanote.Model.Note as MN
import qualified Emanote.Pandoc.Filter.Embed as PF
import qualified Emanote.Pandoc.Filter.Query as PF
import qualified Emanote.Pandoc.Filter.Url as PF
import qualified Emanote.Pandoc.Markdown.Syntax.HashTag as HT
import Emanote.Prelude (h)
import Emanote.Route (FileType (LMLType), LML (Md))
import qualified Emanote.Route as R
import qualified Emanote.Route.SiteRoute as SR
import qualified Emanote.View.LiveServerFiles as LiveServerFiles
import qualified Heist as H
import qualified Heist.Extra.Splices.List as Splices
import qualified Heist.Extra.Splices.Pandoc as Splices
import Heist.Extra.Splices.Pandoc.Render (withoutH1)
import qualified Heist.Extra.Splices.Tree as Splices
import qualified Heist.Extra.TemplateState as Tmpl
import qualified Heist.Interpreted as HI
import qualified Heist.Splices as Heist
import qualified Heist.Splices.Apply as HA
import qualified Heist.Splices.Bind as HB
import qualified Heist.Splices.Json as HJ
import qualified Paths_emanote
import Text.Blaze.Html ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Renderer.XmlHtml as RX
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))

render :: Ema.CLI.Action -> Model -> SR.SiteRoute -> Ema.Asset LByteString
render emaAction m =
  absurdUnion
    `h` ( \(SR.MissingR urlPath) -> do
            let route404 = R.liftLMLRoute @('LMLType 'Md) . coerce $ R.decodeHtmlRoute urlPath
                note404 = MN.mkEmptyNoteWith route404 $ B.Plain [B.Str $ "No note found for '" <> toText urlPath <> "'"]
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
    `h` ( \SR.TagIndexR ->
            Ema.AssetGenerated Ema.Html $ renderSRTagIndex emaAction m
        )
    `h` ( \SR.IndexR ->
            Ema.AssetGenerated Ema.Html $ renderSRIndex emaAction m
        )

renderSRIndex :: Ema.CLI.Action -> Model -> LByteString
renderSRIndex emaAction model = do
  let meta = Meta.getIndexYamlMeta model
  flip (Tmpl.renderHeistTemplate "templates/special/index") (model ^. M.modelHeistTemplate) $ do
    commonSplices emaAction meta "@Index"
    routeTreeSplice Nothing model

renderSRTagIndex :: Ema.CLI.Action -> Model -> LByteString
renderSRTagIndex emaAction model = do
  let meta = Meta.getIndexYamlMeta model
  flip (Tmpl.renderHeistTemplate "templates/special/tagindex") (model ^. M.modelHeistTemplate) $ do
    commonSplices emaAction meta "@Tags"
    "ema:tagindex"
      ## Splices.listSplice (M.modelTags model) "each-tag"
      $ \(tag, notes) -> do
        "tag" ## HI.textSplice (HT.unTag tag)
        "notes"
          ## Splices.listSplice notes "each-note"
          $ \note ->
            PF.noteSplice model note

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
      ## Splices.listSplice (init $ R.routeInits . R.lmlRouteCase $ r) "each-crumb"
      $ \(R.liftLMLRoute -> crumbR) ->
        MapSyntax.mapV HI.textSplice $ do
          "crumb:url" ## Ema.routeUrl model $ SR.lmlSiteRoute crumbR
          "crumb:title" ## M.modelLookupTitle crumbR model
    -- Note stuff
    "ema:note:title"
      ## HI.textSplice pageTitle
    "ema:note:backlinks"
      ## Splices.listSplice (M.modelLookupBacklinks (R.liftModelRoute . R.lmlRouteCase $ r) model) "backlink"
      $ \(source, ctx) -> do
        let ctxDoc :: Pandoc = Pandoc mempty $ one $ B.Div B.nullAttr ctx
        -- TODO: reuse note splice
        "backlink:note:title" ## HI.textSplice (M.modelLookupTitle source model)
        "backlink:note:url" ## HI.textSplice (Ema.routeUrl model $ SR.lmlSiteRoute source)
        "backlink:note:context"
          ## Splices.pandocSplice
            rewriteClass
            (const . const $ Nothing)
            (PF.urlResolvingSplice emaAction model)
          $ ctxDoc
    "ema:note:pandoc"
      ## Splices.pandocSplice
        rewriteClass
        ( \ctx blk ->
            PF.embedWikiLinkResolvingSplice emaAction model ctx blk
              <|> PF.queryResolvingSplice note model ctx blk
        )
        (PF.urlResolvingSplice emaAction model)
      $ note ^. MN.noteDoc
        & withoutH1 -- Because, handling note title separately

commonSplices :: Monad n => Ema.CLI.Action -> Aeson.Value -> Text -> H.Splices (HI.Splice n)
commonSplices emaAction meta routeTitle = do
  let siteTitle = MN.lookupAeson @Text "Emabook Site" ("page" :| ["siteTitle"]) meta
  -- Heist helpers
  "bind" ## HB.bindImpl
  "apply" ## HA.applyImpl
  -- Add tailwind css shim
  "tailwindCssShim"
    ## pure
      (RX.renderHtmlNodes $ twindShim emaAction)
  "ema:version"
    ## HI.textSplice (toText $ showVersion Paths_emanote.version)
  "ema:metadata"
    ## HJ.bindJson meta
  "ema:title" ## HI.textSplice routeTitle
  "ema:titleFull"
    ## HI.textSplice
    $ if routeTitle == siteTitle
      then siteTitle
      else routeTitle <> " – " <> siteTitle
  where
    twindShim :: Ema.CLI.Action -> H.Html
    twindShim action =
      case action of
        Ema.CLI.Generate _ ->
          Tailwind.twindShimUnofficial
        _ ->
          -- Twind shim doesn't reliably work in dev server mode. Let's just use the
          -- tailwind CDN.
          H.link
            ! A.href (H.toValue LiveServerFiles.tailwindFullCssUrl)
            ! A.rel "stylesheet"
            ! A.type_ "text/css"

-- | If there is no 'current route', all sub-trees are marked as active/open.
routeTreeSplice :: Monad n => Maybe R.LMLRoute -> Model -> H.Splices (HI.Splice n)
routeTreeSplice mr model = do
  "ema:route-tree"
    ## ( let tree = PathTree.treeDeleteChild "index" $ model ^. M.modelNav
             getOrder tr =
               ( Meta.lookupRouteMeta @Int 0 (one "order") tr model,
                 maybe (R.routeBaseName . R.lmlRouteCase $ tr) MN.noteTitle $ M.modelLookupNoteByRoute tr model
               )
             getCollapsed tr =
               Meta.lookupRouteMeta @Bool True ("template" :| ["sidebar", "collapsed"]) tr model
             mkLmlRoute = R.liftLMLRoute . R.R @('LMLType 'Md)
             lmlRouteSlugs = R.unRoute . R.lmlRouteCase
          in Splices.treeSplice (getOrder . mkLmlRoute) tree $ \(mkLmlRoute -> nodeRoute) children -> do
               "node:text" ## HI.textSplice $ M.modelLookupTitle nodeRoute model
               "node:url" ## HI.textSplice $ Ema.routeUrl model $ SR.lmlSiteRoute nodeRoute
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
