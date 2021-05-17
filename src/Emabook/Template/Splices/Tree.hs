{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Emabook.Template.Splices.Tree (treeSplice) where

import Data.Map.Syntax ((##))
import Data.Tree (Tree (..))
import qualified Heist as H
import qualified Heist.Interpreted as HI
import qualified Heist.Splices as Heist

treeSplice ::
  (Monad n, Ord sortKey) => [a] -> (NonEmpty a -> sortKey) -> [Tree a] -> (NonEmpty a -> H.Splices (HI.Splice n)) -> HI.Splice n
treeSplice pars sortKey trees childSplice = do
  let treeSorted = flip sortOn trees $ \(rootLabel -> x) ->
        sortKey $ maybe (one x) (<> one x) $ nonEmpty pars
  flip foldMapM treeSorted $ \(Node lbl children) -> do
    HI.runChildrenWith $ do
      let herePath = maybe (one lbl) (<> one lbl) $ nonEmpty pars
      childSplice herePath
      "has-children" ## Heist.ifElseISplice (not . null $ children)
      let childrenSorted = sortOn (sortKey . (herePath <>) . one . rootLabel) children
      "children"
        ## treeSplice (toList herePath) sortKey childrenSorted childSplice
