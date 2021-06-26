{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- TODO: Split this sensibly.
module Emanote.View.TagIndex (renderSRTagIndex) where

import Control.Lens.Operators ((^.))
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Map.Syntax ((##))
import qualified Data.Text as T
import Data.Tree (Forest, Tree)
import qualified Data.Tree as Tree
import qualified Ema.CLI
import Emanote.Model (Model)
import qualified Emanote.Model as M
import qualified Emanote.Model.Meta as Meta
import qualified Emanote.Model.Note as MN
import qualified Emanote.Model.Title as Tit
import qualified Emanote.Pandoc.Filter.Query as PF
import qualified Emanote.Pandoc.Markdown.Syntax.HashTag as HT
import Emanote.View.Common (commonSplices)
import qualified Heist.Extra.Splices.List as Splices
import qualified Heist.Extra.TemplateState as Tmpl
import qualified Heist.Interpreted as HI

data TagIndex = TagIndex
  { tagIndexPath :: [HT.TagNode],
    tagIndexTitle :: Text,
    tagIndexNotes :: [MN.Note],
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
          TagIndex [] "Tag Index" [] childTags
        Just tagPath ->
          let notes =
                snd . Tree.rootLabel $ lookupForestMust tagPath tagForest
           in TagIndex (toList tagPath) ("#" <> tagNodesText tagPath <> " - Tag Index") notes childTags
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

renderSRTagIndex :: Ema.CLI.Action -> Model -> [HT.TagNode] -> LByteString
renderSRTagIndex emaAction model tagPath = do
  -- TODO: Implement tagPath-based rendering, including:
  -- - [x] Tag breadcrumbs
  -- - [ ] Note count (and tag child count)
  -- - [ ] Tag links from elsewhere
  let meta = Meta.getIndexYamlMeta model
      TagIndex {..} = mkTagIndex model tagPath
  flip (Tmpl.renderHeistTemplate "templates/special/tagindex") (model ^. M.modelHeistTemplate) $ do
    commonSplices emaAction meta $ Tit.fromPlain tagIndexTitle
    "ema:tag:title" ## HI.textSplice (maybe "/" (HT.unTagNode . last) $ nonEmpty tagPath)
    "ema:tag:url" ## HI.textSplice (tagNodesUrl tagPath)
    let parents = maybe [] (inits . init) $ nonEmpty tagIndexPath
    "ema:tagcrumbs" ## Splices.listSplice parents "ema:each-crumb" $
      \crumb -> do
        let crumbTitle = maybe "/" (HT.unTagNode . last) . nonEmpty $ crumb
            crumbUrl = tagNodesUrl crumb
        "ema:tagcrumb:title" ## HI.textSplice crumbTitle
        "ema:tagcrumb:url" ## HI.textSplice crumbUrl
    "ema:childTags"
      ## Splices.listSplice tagIndexChildren "ema:each-childTag"
      $ \childTag -> do
        "ema:childTag:title" ## HI.textSplice (tagNodesText $ fst childTag)
        "ema:childTag:url" ## HI.textSplice (tagNodesUrl (toList $ fst childTag))
        -- TODO: Also subtag count
        "ema:childTag:count" ## HI.textSplice (show (length $ snd childTag))
    "ema:notes"
      ## Splices.listSplice tagIndexNotes "ema:each-note"
      $ \note ->
        PF.noteSplice model note
  where
    -- TODO: Use SiteRoute encoder
    tagNodesUrl =
      ("-/tags" `concatUrl`) . T.intercalate "/" . fmap HT.unTagNode
    concatUrl :: Text -> Text -> Text
    concatUrl a "" = a
    concatUrl a b = a <> "/" <> b

tagNodesText :: NonEmpty HT.TagNode -> Text
tagNodesText =
  HT.unTag . HT.constructTag
