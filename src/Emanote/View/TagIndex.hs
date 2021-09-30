{-# LANGUAGE TypeApplications #-}

module Emanote.View.TagIndex (renderTagIndex) where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Map.Syntax ((##))
import Data.Tree (Forest, Tree)
import qualified Data.Tree as Tree
import Emanote.Model (Model)
import qualified Emanote.Model as M
import qualified Emanote.Model.Meta as Meta
import qualified Emanote.Model.Note as MN
import qualified Emanote.Pandoc.Markdown.Syntax.HashTag as HT
import qualified Emanote.Pandoc.Renderer.Query as PF
import qualified Emanote.Route.SiteRoute.Class as SR
import Emanote.View.Common (commonSplices, inlineRenderers, mkRendererFromMeta, renderModelTemplate)
import qualified Heist.Extra.Splices.List as Splices
import Heist.Extra.Splices.Pandoc.Ctx (emptyRenderCtx)
import qualified Heist.Interpreted as HI
import Relude

-- An index view into the notebook indexed by the given tag path.
data TagIndex = TagIndex
  { -- | The tag path under which this index is creatd
    tagIndexPath :: [HT.TagNode],
    -- | User descriptive title of this index
    tagIndexTitle :: Text,
    -- | All notes tagged precisely with this tag path
    tagIndexNotes :: [MN.Note],
    -- | Tags immediately under this tag path.
    --
    -- If the tag path being index is "foo/bar", this will contain "foo/bar/qux".
    tagIndexChildren :: [(NonEmpty HT.TagNode, [MN.Note])]
  }
  deriving (Eq)

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
           in (t, fromMaybe mempty $ Map.lookup (HT.constructTag t) tagMap)
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
  let meta = Meta.getIndexYamlMeta model
      withNoteRenderer = mkRendererFromMeta model meta
      withInlineCtx =
        withNoteRenderer inlineRenderers () ()
      tagIdx = mkTagIndex model tagPath
  renderModelTemplate model "templates/special/tagindex" $ do
    commonSplices ($ emptyRenderCtx) model meta $ fromString . toString $ tagIndexTitle tagIdx
    "ema:tag:title" ## HI.textSplice (maybe "/" (HT.unTagNode . last) $ nonEmpty tagPath)
    "ema:tag:url" ## HI.textSplice (SR.siteRouteUrl model $ SR.tagIndexRoute tagPath)
    let parents = maybe [] (inits . init) $ nonEmpty (tagIndexPath tagIdx)
    "ema:tagcrumbs" ## Splices.listSplice parents "ema:each-crumb" $
      \crumb -> do
        let crumbTitle = maybe "/" (HT.unTagNode . last) . nonEmpty $ crumb
            crumbUrl = SR.siteRouteUrl model $ SR.tagIndexRoute crumb
        "ema:tagcrumb:title" ## HI.textSplice crumbTitle
        "ema:tagcrumb:url" ## HI.textSplice crumbUrl
    "ema:childTags"
      ## Splices.listSplice (tagIndexChildren tagIdx) "ema:each-childTag"
      $ \childTag -> do
        let childIndex = mkTagIndex model (toList . fst $ childTag)
        "ema:childTag:title" ## HI.textSplice (tagNodesText $ fst childTag)
        "ema:childTag:url" ## HI.textSplice (SR.siteRouteUrl model $ SR.tagIndexRoute (toList $ fst childTag))
        "ema:childTag:count-note" ## HI.textSplice (show (length $ snd childTag))
        "ema:childTag:count-tag" ## HI.textSplice (show (length $ tagIndexChildren childIndex))
    "ema:notes"
      ## Splices.listSplice (tagIndexNotes tagIdx) "ema:each-note"
      $ \note ->
        PF.noteSpliceMap withInlineCtx model note

tagNodesText :: NonEmpty HT.TagNode -> Text
tagNodesText =
  HT.unTag . HT.constructTag
