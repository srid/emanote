module Emanote.View.TagIndex (renderTagIndex) where

import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Map.Syntax ((##))
import Data.Tree (Forest, Tree)
import Data.Tree qualified as Tree
import Emanote.Model (Model)
import Emanote.Model qualified as M
import Emanote.Model.Note qualified as MN
import Emanote.Pandoc.Markdown.Syntax.HashTag qualified as HT
import Emanote.Pandoc.Renderer.Query qualified as PF
import Emanote.Route.SiteRoute.Class qualified as SR
import Emanote.View.Common (
  TemplateRenderCtx (withInlineCtx),
  commonSplices,
  defaultRouteMeta,
  mkTemplateRenderCtx,
  renderModelTemplate,
 )
import Heist.Extra.Splices.List qualified as Splices
import Heist.Extra.Splices.Pandoc.Ctx (emptyRenderCtx)
import Heist.Interpreted qualified as HI
import Relude

-- An index view into the notebook indexed by the given tag path.
data TagIndex = TagIndex
  { tagIndexPath :: [HT.TagNode]
  -- ^ The tag path under which this index is creatd
  , tagIndexTitle :: Text
  -- ^ User descriptive title of this index
  , tagIndexNotes :: [MN.Note]
  -- ^ All notes tagged precisely with this tag path
  , tagIndexChildren :: [(NonEmpty HT.TagNode, [MN.Note])]
  -- ^ Tags immediately under this tag path.
  --
  -- If the tag path being index is "foo/bar", this will contain "foo/bar/qux".
  }
  deriving stock (Eq)

mkTagIndex :: Model -> [HT.TagNode] -> TagIndex
mkTagIndex model tagPath' =
  let mTagPath = nonEmpty tagPath'
      tagMap = Map.fromList $ M.modelTags model
      tagForest = HT.tagTree tagMap
      childNodes =
        maybe
          (fst . Tree.rootLabel <$> tagForest)
          (fmap (fst . Tree.rootLabel) . Tree.subForest . flip lookupForestMust tagForest)
          mTagPath
      childTags =
        childNodes <&> \childNode ->
          let t = NE.reverse $ childNode :| reverse tagPath'
           in (t, maybeToMonoid $ Map.lookup (HT.constructTag t) tagMap)
   in case mTagPath of
        Nothing ->
          -- The root index displays all top-level tags (no notes)
          TagIndex [] "Tag Index" [] childTags
        Just tagPath ->
          let notes =
                snd . Tree.rootLabel $ lookupForestMust tagPath tagForest
              viewTitle = "#" <> tagNodesText tagPath <> " - Tag Index"
           in TagIndex (toList tagPath) viewTitle notes childTags
  where
    lookupForestMust :: (Show k, Eq k) => NonEmpty k -> Forest (k, a) -> Tree (k, a)
    lookupForestMust path =
      fromMaybe (error $ "Tag not found in forest: " <> show path)
        . lookupForest path
    lookupForest :: Eq k => NonEmpty k -> Forest (k, a) -> Maybe (Tree (k, a))
    lookupForest (k :| ks') trees =
      case nonEmpty ks' of
        Nothing ->
          List.find (\(Tree.Node lbl _) -> fst lbl == k) trees
        Just ks -> do
          subForest <- Tree.subForest <$> List.find (\(Tree.Node lbl _) -> fst lbl == k) trees
          lookupForest ks subForest

renderTagIndex :: Model -> [HT.TagNode] -> LByteString
renderTagIndex model tagPath = do
  let (r, meta) = defaultRouteMeta model
      tCtx = mkTemplateRenderCtx model r meta
      tagIdx = mkTagIndex model tagPath
  renderModelTemplate model "templates/special/tagindex" $ do
    commonSplices ($ emptyRenderCtx) model meta $ fromString . toString $ tagIndexTitle tagIdx
    "ema:tag:title" ## HI.textSplice (maybe "/" (HT.unTagNode . last) $ nonEmpty tagPath)
    "ema:tag:url" ## HI.textSplice (SR.siteRouteUrl model $ SR.tagIndexRoute tagPath)
    let parents = maybe [] (inits . init) $ nonEmpty (tagIndexPath tagIdx)
    "ema:tagcrumbs" ##
      Splices.listSplice parents "ema:each-crumb" $
        \crumb -> do
          let crumbTitle = maybe "/" (HT.unTagNode . last) . nonEmpty $ crumb
              crumbUrl = SR.siteRouteUrl model $ SR.tagIndexRoute crumb
          "ema:tagcrumb:title" ## HI.textSplice crumbTitle
          "ema:tagcrumb:url" ## HI.textSplice crumbUrl
    "ema:childTags" ##
      Splices.listSplice (tagIndexChildren tagIdx) "ema:each-childTag" $
        \childTag -> do
          let childIndex = mkTagIndex model (toList . fst $ childTag)
          "ema:childTag:title" ## HI.textSplice (tagNodesText $ fst childTag)
          "ema:childTag:url" ## HI.textSplice (SR.siteRouteUrl model $ SR.tagIndexRoute (toList $ fst childTag))
          "ema:childTag:count-note" ## HI.textSplice (show (length $ snd childTag))
          "ema:childTag:count-tag" ## HI.textSplice (show (length $ tagIndexChildren childIndex))
    "ema:notes" ##
      Splices.listSplice (tagIndexNotes tagIdx) "ema:each-note" $
        \note ->
          PF.noteSpliceMap (withInlineCtx tCtx) model note

tagNodesText :: NonEmpty HT.TagNode -> Text
tagNodesText =
  HT.unTag . HT.constructTag
