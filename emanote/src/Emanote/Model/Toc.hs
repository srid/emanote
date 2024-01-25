module Emanote.Model.Toc where

import Data.Map.Syntax ((##))
import Data.Tree (Tree (Node))
import Data.Tree qualified as Tree
import Heist qualified as H
import Heist.Extra (runCustomNode)
import Heist.Extra.Splices.Pandoc (RenderCtx (rootNode))
import Heist.Interpreted qualified as HI
import Relude
import Text.Pandoc
import Text.Pandoc.Shared (stringify)
import Text.XmlHtml qualified as X

type Toc = Tree.Forest DocHeading

data DocHeading = DocHeading
  { headingId :: Text
  , headingName :: Text
  }
  deriving stock (Show, Eq)

-- | Collect the heading and their level
pandocToHeadings :: Pandoc -> [(Int, DocHeading)]
pandocToHeadings (Pandoc _ blocks) = mapMaybe toHeading blocks
  where
    toHeading block = case block of
      Header hlvl (oid, _, _) inlines -> Just (hlvl, DocHeading oid (stringify inlines))
      _ -> Nothing

{- | Create the Toc
TODO: figure out the base heading, sometime it's 1, sometime it's 2
-}
newToc :: Pandoc -> Toc
newToc = go [] 2 . pandocToHeadings
  where
    go acc lvl ((headingLvl, heading) : rest)
      | lvl == headingLvl =
          let
            -- collect following headings that are childs
            childs = go [] (lvl + 1) rest
            newAcc = Tree.Node heading childs : acc
            childCount = sum $ map length childs
           in
            go newAcc lvl (drop childCount rest)
    go acc _ _ = reverse acc

-- Note: this is inspired by 'Heist.Extra.Splices.Pandoc.Footnotes.renderFootnotesWith'
renderToc :: RenderCtx -> Toc -> HI.Splice Identity
renderToc ctx toc =
  fromMaybe (pure []) $ do
    renderNode <- viaNonEmpty head $ maybe [] (X.childElementsTag "Toc:List") $ rootNode ctx
    Just
      $ runCustomNode renderNode
      $ do
        "toc" ##
          (HI.runChildrenWith . (tocSplices ctx)) `foldMapM` toc

tocSplices :: RenderCtx -> Tree DocHeading -> H.Splices (HI.Splice Identity)
tocSplices ctx (Node heading childs) = do
  "toc:title" ## HI.textSplice (headingName heading)
  "toc:ref" ## HI.textSplice (headingId heading)
  "toc:childs" ## renderToc ctx childs
