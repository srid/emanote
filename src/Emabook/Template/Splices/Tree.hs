{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Emabook.Template.Splices.Tree (treeSplice) where

import Data.Map.Syntax ((##))
import Data.Tree (Tree (Node))
import qualified Heist as H
import qualified Heist.Interpreted as HI
import qualified Heist.Splices as Heist

treeSplice ::
  Monad n => [a] -> [Tree a] -> (NonEmpty a -> H.Splices (HI.Splice n)) -> HI.Splice n
treeSplice pars trees childSplice = do
  flip foldMapM trees $ \(Node lbl children) -> do
    HI.runChildrenWith $ do
      let herePath = maybe (one lbl) (<> one lbl) $ nonEmpty pars
      childSplice herePath
      "has-children" ## Heist.ifElseISplice (not . null $ children)
      "children"
        ## treeSplice (toList herePath) children childSplice
