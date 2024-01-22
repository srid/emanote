module Emanote.Model.Toc where

import Data.Tree qualified as Tree
import Relude
import Text.Pandoc
import Text.Pandoc.Shared (stringify)

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

-- | Create the Toc
newToc :: Pandoc -> Toc
newToc = go [] 1 . pandocToHeadings
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
