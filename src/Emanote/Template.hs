{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.Template (render) where

import Control.Lens.Operators ((^.))
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
import Emanote.Class (EmanoteRoute (..))
import qualified Emanote.Class as C
import Emanote.Model (Model)
import qualified Emanote.Model as M
import qualified Emanote.Model.Meta as Meta
import qualified Emanote.Model.Note as MN
import qualified Emanote.Model.Query as Q
import qualified Emanote.Model.Rel as Rel
import qualified Emanote.Model.StaticFile as SF
import qualified Emanote.Prelude as EP
import Emanote.Route (FileType (Html, LMLType), LML (Md), R)
import qualified Emanote.Route as R
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

render :: Ema.CLI.Action -> Model -> EmanoteRoute -> Ema.Asset LByteString
render emaAction m = \case
  EROtherFile (_r, fpAbs) ->
    Ema.AssetStatic fpAbs
  ERNoteHtml (mdRouteForHtmlRoute -> r) ->
    Ema.AssetGenerated Ema.Html $ renderLmlHtml emaAction m r
  ERIndex ->
    Ema.AssetGenerated Ema.Html $ renderIndex emaAction m
  where
    mdRouteForHtmlRoute :: R 'Html -> R.LinkableLMLRoute
    mdRouteForHtmlRoute = R.liftLinkableLMLRoute . coerce @(R 'Html) @(R ('LMLType 'Md))

renderIndex :: Ema.CLI.Action -> Model -> LByteString
renderIndex emaAction model = do
  let meta = Meta.getIndexYamlMeta model
  flip (Tmpl.renderHeistTemplate "templates/special/index") (model ^. M.modelHeistTemplate) $ do
    commonSplices emaAction meta
    -- TODO: Style tree differently in index
    routeTreeSplice Nothing model

commonSplices :: Monad n => Ema.CLI.Action -> Aeson.Value -> H.Splices (HI.Splice n)
commonSplices emaAction meta = do
  -- Heist helpers
  "bind" ## HB.bindImpl
  "apply" ## HA.applyImpl
  -- Add tailwind css shim
  "tailwindCssShim"
    ## pure (RX.renderHtmlNodes $ Tailwind.twindShim emaAction)
  -- TODO: Add site title
  "ema:metadata"
    ## HJ.bindJson meta

-- | If there is no 'current route', all sub-trees are marked as active/open.
routeTreeSplice :: Monad n => Maybe R.LinkableLMLRoute -> Model -> H.Splices (HI.Splice n)
routeTreeSplice mr model = do
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
               let isActiveNode = Just nodeRoute == mr
                   isActiveTree =
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

renderLmlHtml :: Ema.CLI.Action -> Model -> R.LinkableLMLRoute -> LByteString
renderLmlHtml emaAction model r = do
  let meta = Meta.getEffectiveRouteMeta r model
      templateName = MN.lookupAeson @Text "templates/_default" ("template" :| ["name"]) meta
      rewriteClass = MN.lookupAeson @(Map Text Text) mempty ("pandoc" :| ["rewriteClass"]) meta
      siteTitle = MN.lookupAeson @Text "Emabook Site" ("page" :| ["siteTitle"]) meta
      pageTitle = M.modelLookupTitle r model
  flip (Tmpl.renderHeistTemplate templateName) (model ^. M.modelHeistTemplate) $ do
    commonSplices emaAction meta
    -- Sidebar navigation
    routeTreeSplice (Just r) model
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
      ## Splices.pandocSpliceWithCustomClass
        rewriteClass
        (querySplice model)
        (const . const $ Nothing)
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
      EP.rewriteLinks (resolveUrl emaAction model)

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
          ## (HI.runChildrenWith . noteSplice) `foldMapM` Q.runQuery model q
  pure $
    H.localHS (HI.bindSplices splices) $
      HI.runNode queryNode
  where
    childElementTagWithClass tag mCls node = do
      queryNodes <- nonEmpty $ X.childElementsTag tag node
      fmap head . nonEmpty $
        NE.filter ((== mCls) . X.getAttribute "class") queryNodes

-- TODO: Reuse this elsewhere
noteSplice :: Monad n => MN.Note -> H.Splices (HI.Splice n)
noteSplice note = do
  "note:title" ## HI.textSplice (MN.noteTitle note)
  "note:url" ## HI.textSplice (Ema.routeUrl $ C.lmlHtmlRoute $ note ^. MN.noteRoute)
  "note:metadata" ## HJ.bindJson (note ^. MN.noteMeta)

resolveUrl :: Ema.CLI.Action -> Model -> [(Text, Text)] -> ([B.Inline], Text) -> Either Text ([B.Inline], Text)
resolveUrl emaAction model linkAttrs x@(inner, url) =
  fromMaybe (Right x) $ do
    res <- resolveRelTarget model <=< Rel.parseRelTarget linkAttrs $ url
    pure $ do
      (r, mTime) <- res
      let mNewInner = do
            -- Automatic link text replacement is done only if the user has not set
            -- a custom link text.
            guard $ plainify inner == url
            case r of
              ERNoteHtml htmlR -> do
                let nr = R.liftLinkableLMLRoute $ coerce @(R 'Html) @(R ('R.LMLType 'R.Md)) htmlR
                one . B.Str . MN.noteTitle <$> M.modelLookupNote nr model
              EROtherFile _ -> do
                -- Just append a file: prefix.
                pure $ B.Str "File: " : inner
              ERIndex ->
                Nothing
          queryString =
            fromMaybe "" $ do
              -- In live server mode, append last modification time if any, such
              -- that the browser is forced to refresh the inline image on hot
              -- reload (Ema's DOM patch).
              guard $ emaAction == Ema.CLI.Run
              t <- mTime
              pure $ toText $ "?t=" <> formatTime defaultTimeLocale "%s" t
      pure (fromMaybe inner mNewInner, Ema.routeUrl r <> queryString)

resolveRelTarget :: Model -> Rel.RelTarget -> Maybe (Either Text (EmanoteRoute, Maybe UTCTime))
resolveRelTarget model = \case
  Right r ->
    Right <$> resolveLinkableRoute r
  Left wl ->
    case nonEmpty (M.modelLookupRouteByWikiLink wl model) of
      Nothing -> do
        pure $ Left "Unresolved link"
      Just targets ->
        -- TODO: Deal with ambiguous targets here
        Right <$> resolveLinkableRoute (head targets)
  where
    resolveLinkableRoute r =
      case R.linkableRouteCase r of
        Left mdR ->
          -- NOTE: Because don't support slugs yet.
          pure (C.lmlHtmlRoute mdR, Nothing)
        Right sR -> do
          staticFile <- M.modelLookupStaticFileByRoute sR model
          pure (C.staticFileRoute staticFile, Just $ staticFile ^. SF.staticFileTime)
