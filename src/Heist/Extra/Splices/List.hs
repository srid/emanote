module Heist.Extra.Splices.List where

import Data.Map.Syntax ((##))
import Heist qualified as H
import Heist.Interpreted qualified as HI
import Relude

-- | A splice that applies a non-empty list
listSplice :: [a] -> Text -> (a -> H.Splices (HI.Splice Identity)) -> HI.Splice Identity
listSplice xs childTag childSplice = do
  if null xs
    then pure mempty
    else HI.runChildrenWith $ do
      childTag
        ## (HI.runChildrenWith . childSplice)
          `foldMapM` xs
