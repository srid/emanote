{-# LANGUAGE TemplateHaskell #-}

module Spec (main) where

import Emanote.Model.Link.RelSpec qualified as RelSpec
import Emanote.Model.QuerySpec qualified as QuerySpec
import Hedgehog.Main (defaultMain)
import Relude

main :: IO ()
main = do
  defaultMain
    [ QuerySpec.tests
    , RelSpec.tests
    ]
