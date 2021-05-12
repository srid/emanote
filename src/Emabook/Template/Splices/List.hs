{-# LANGUAGE TypeApplications #-}

module Emabook.Template.Splices.List where

import Data.Map.Syntax ((##))
import qualified Heist as H
import qualified Heist.Interpreted as HI

-- | A splice that applies a non-empty list
listSplice :: Monad n => [a] -> Text -> (a -> H.Splices (HI.Splice n)) -> HI.Splice n
listSplice xs childTag childSplice = do
  if null xs
    then pure mempty
    else HI.runChildrenWith $ do
      childTag
        ## (HI.runChildrenWith . childSplice)
          `foldMapM` xs
