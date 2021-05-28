{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.Template (render) where

import Control.Lens.Operators ((^.))
import qualified Data.List.NonEmpty as NE
import Data.Map.Syntax ((##))
import qualified Data.Map.Syntax as MapSyntax
import qualified Ema
import qualified Ema.Helper.PathTree as PathTree
import Emanote.Class (EmanoteRoute (..))
import qualified Emanote.Class as C
import Emanote.Model (Model)
import qualified Emanote.Model as M
import qualified Emanote.Model.Meta as Meta
import qualified Emanote.Model.Note as MN
import qualified Emanote.Model.Rel as Rel
import qualified Emanote.Prelude as EP
import Emanote.Route (FileType (Html, LMLType), LML (Md), R)
import qualified Emanote.Route as R
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
    mdRouteForHtmlRoute :: R 'Html -> R.LinkableLMLRoute
    mdRouteForHtmlRoute = R.liftLinkableLMLRoute . coerce @(R 'Html) @(R ('LMLType 'Md))

renderHtml :: H.Html -> Model -> R.LinkableLMLRoute -> LByteString
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
                 ( Meta.lookupMeta @Int 0 (one "order") tr model,
                   maybe (R.routeBaseName . R.someLinkableLMLRouteCase $ tr) MN.noteTitle $ M.modelLookupNote tr model
                 )
               getCollapsed tr =
                 Meta.lookupMeta @Bool True ("template" :| ["sidebar", "collapsed"]) tr model
               mkLmlRoute = R.liftLinkableLMLRoute . R.R @('LMLType 'Md)
               lmlRouteSlugs = R.unRoute . R.someLinkableLMLRouteCase
            in Splices.treeSplice (getOrder . mkLmlRoute) tree $ \(mkLmlRoute -> nodeRoute) children -> do
                 "node:text" ## HI.textSplice $ M.modelLookupTitle nodeRoute model
                 "node:url" ## HI.textSplice $ Ema.routeUrl $ C.lmlHtmlRoute nodeRoute
                 let isActiveNode = nodeRoute == r
                     isActiveTree =
                       toList (lmlRouteSlugs nodeRoute) `NE.isPrefixOf` lmlRouteSlugs r
                     openTree =
                       isActiveTree -- Active tree is always open
                         || not (getCollapsed nodeRoute)
                 "node:active" ## Heist.ifElseISplice isActiveNode
                 "node:terminal" ## Heist.ifElseISplice (null children)
                 "tree:childrenCount" ## HI.textSplice (show $ length children)
                 "tree:open" ## Heist.ifElseISplice openTree
         )
    "ema:breadcrumbs"
      ## Splices.listSplice (init $ R.routeInits . R.someLinkableLMLRouteCase $ r) "each-crumb"
      $ \(R.liftLinkableLMLRoute -> crumbR) ->
        MapSyntax.mapV HI.textSplice $ do
          "crumb:url" ## Ema.routeUrl $ C.lmlHtmlRoute crumbR
          "crumb:title" ## M.modelLookupTitle crumbR model
    -- Note stuff
    "ema:note:title"
      ## HI.textSplice pageTitle
    "ema:note:titleFull"
      ## HI.textSplice (if pageTitle == siteTitle then pageTitle else pageTitle <> " – " <> siteTitle)
    "ema:note:backlinks"
      ## Splices.listSplice (M.modelLookupBacklinks (R.liftLinkableRoute . R.someLinkableLMLRouteCase $ r) model) "backlink"
      $ \(source, ctx) -> do
        let ctxDoc :: Pandoc = Pandoc mempty $ one $ B.Div B.nullAttr ctx
        -- TODO: reuse note splice
        "backlink:note:title" ## HI.textSplice (M.modelLookupTitle source model)
        "backlink:note:url" ## HI.textSplice (Ema.routeUrl $ C.lmlHtmlRoute source)
        "backlink:note:context"
          ## Splices.pandocSplice
          $ ctxDoc & resolvePandoc
    "ema:note:pandoc"
      ## Splices.pandocSpliceWithCustomClass rewriteClass
      $ case M.modelLookupNote r model of
        Nothing ->
          -- This route doesn't correspond to any Markdown file on disk. Could be one of the reasons,
          -- 1. Refers to a folder route (and no ${folder}.md exists)
          -- 2. A broken wiki-links
          -- In both cases, we take the lenient approach, and display an empty page (but with title).
          -- TODO: Display folder children if this is a folder note. It is hinted to in the sidebar too.
          Pandoc mempty $ one $ B.Plain $ one $ B.Str "No Markdown file exists for this route."
        Just note ->
          note ^. MN.noteDoc
            & ( EP.withoutH1 -- Because, handling note title separately
                  >>> resolvePandoc
              )
  where
    resolvePandoc =
      EP.rewriteLinks (resolveUrl model)

-- | Convert .md or wiki links to their proper route url.
--
-- Requires resolution from the `model` state. Late resolution, in other words.
resolveUrl :: Model -> [(Text, Text)] -> Text -> Either Text Text
resolveUrl model linkAttrs url =
  fromMaybe (Right url) $
    fmap (fmap Ema.routeUrl) . resolveRelTarget model
      <=< Rel.parseRelTarget linkAttrs
      $ url

resolveRelTarget :: Model -> Rel.RelTarget -> Maybe (Either Text EmanoteRoute)
resolveRelTarget model = \case
  Right r ->
    Right <$> resolveLinkableRoute r
  Left wl ->
    case nonEmpty (M.modelLookupRouteByWikiLink wl model) of
      Nothing -> do
        pure $ Left "Unresolved link"
      Just targets ->
        -- TODO: Deal with ambiguous targets here
        fmap Right $ resolveLinkableRoute $ head targets
  where
    resolveLinkableRoute r =
      case R.linkableRouteCase r of
        Left mdR ->
          -- NOTE: Because don't support slugs yet.
          pure $ C.lmlHtmlRoute mdR
        Right sR ->
          C.staticFileRoute <$> M.modelLookupStaticFileByRoute sR model
