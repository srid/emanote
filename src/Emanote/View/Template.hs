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
import qualified Ema
import qualified Ema.CLI
import qualified Ema.Helper.PathTree as PathTree
import qualified Ema.Helper.Tailwind as Tailwind
import Emanote.Model (Model)
import qualified Emanote.Model as M
import qualified Emanote.Model.Meta as Meta
import qualified Emanote.Model.Note as MN
import qualified Emanote.Pandoc.Filter.Query as PF
import qualified Emanote.Pandoc.Filter.Url as PF
import qualified Emanote.Pandoc.Markdown.Syntax.HashTag as HT
import Emanote.Route (FileType (LMLType), LML (Md))
import qualified Emanote.Route as R
import Emanote.View.SiteRoute (SiteRoute (..))
import qualified Emanote.View.SiteRoute as SR
import qualified Heist as H
import qualified Heist.Extra.Splices.List as Splices
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

render :: Ema.CLI.Action -> Model -> SiteRoute -> Ema.Asset LByteString
render emaAction m = \case
  SRStaticFile (_r, fpAbs) ->
    Ema.AssetStatic fpAbs
  SRLMLFile lmlRoute -> do
    case M.modelLookupNoteByRoute lmlRoute m of
      Just note ->
        Ema.AssetGenerated Ema.Html $ renderLmlHtml emaAction m note
      Nothing ->
        -- This should never be reached because decodeRoute looks up the model.
        error $ "Bad route: " <> show lmlRoute
  SRIndex ->
    Ema.AssetGenerated Ema.Html $ rendeSRIndex emaAction m
  SRTagIndex ->
    Ema.AssetGenerated Ema.Html $ rendeSRTagIndex emaAction m
  SR404 urlPath -> do
    let route404 = R.liftLinkableLMLRoute @('LMLType 'Md) . coerce $ R.decodeHtmlRoute urlPath
        note404 = MN.mkEmptyNoteWith route404 $ B.Plain [B.Str $ "No note found for '" <> toText urlPath <> "'"]
    Ema.AssetGenerated Ema.Html $ renderLmlHtml emaAction m note404

rendeSRIndex :: Ema.CLI.Action -> Model -> LByteString
rendeSRIndex emaAction model = do
  let meta = Meta.getIndexYamlMeta model
  flip (Tmpl.renderHeistTemplate "templates/special/index") (model ^. M.modelHeistTemplate) $ do
    commonSplices emaAction meta "@Index"
    routeTreeSplice Nothing model

rendeSRTagIndex :: Ema.CLI.Action -> Model -> LByteString
rendeSRTagIndex emaAction model = do
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
            (PF.urlResolvingSplice emaAction model)
          $ ctxDoc
    "ema:note:pandoc"
      ## Splices.pandocSpliceWithCustomClass
        rewriteClass
        (PF.queryResolvingSplice model)
        (PF.urlResolvingSplice emaAction model)
      $ note ^. MN.noteDoc
        & withoutH1 -- Because, handling note title separately
  where
    withoutH1 :: Pandoc -> Pandoc
    withoutH1 (Pandoc meta (B.Header 1 _ _ : rest)) =
      Pandoc meta rest
    withoutH1 doc =
      doc

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
