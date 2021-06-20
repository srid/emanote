module Heist.Extra where

import qualified Heist as H
import qualified Heist.Interpreted as HI
import qualified Text.XmlHtml as X

-- | Useful for running a splice against an arbitrary node (such as that pulled from pandoc.tpl)
runCustomNode :: Monad n => X.Node -> H.Splices (HI.Splice n) -> HI.Splice n
runCustomNode node splices =
  H.localHS (HI.bindSplices splices) $ do
    HI.runNode node <&> \case
      [resNode]
        | X.elementTag resNode == X.elementTag node ->
          -- Get rid of the `node` itself.
          X.elementChildren resNode
      res ->
        res
