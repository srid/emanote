module Heist.Extra.Splices.Tree (treeSplice) where

import Data.Map.Syntax ((##))
import Data.Tree (Tree (..))
import Heist qualified as H
import Heist.Interpreted qualified as HI
import Heist.Splices qualified as Heist
import Relude

treeSplice ::
  forall a n sortKey.
  (Monad n, Ord sortKey) =>
  -- | How to sort children
  (NonEmpty a -> sortKey) ->
  -- | Input tree
  [Tree a] ->
  -- | How to render a (sub-)tree root
  (NonEmpty a -> [Tree a] -> H.Splices (HI.Splice n)) ->
  HI.Splice n
treeSplice =
  go []
  where
    go :: [a] -> (NonEmpty a -> sortKey) -> [Tree a] -> (NonEmpty a -> [Tree a] -> H.Splices (HI.Splice n)) -> HI.Splice n
    go pars sortKey trees childSplice = do
      let extendPars x = maybe (one x) (<> one x) $ nonEmpty pars
      flip foldMapM (sortOn (sortKey . extendPars . rootLabel) trees) $ \(Node lbl children) -> do
        HI.runChildrenWith $ do
          let herePath = extendPars lbl
          childSplice herePath children
          "has-children" ## Heist.ifElseISplice (not . null $ children)
          let childrenSorted = sortOn (sortKey . (herePath <>) . one . rootLabel) children
          "children"
            ## go (toList herePath) sortKey childrenSorted childSplice
