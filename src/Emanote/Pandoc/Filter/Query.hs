{-# LANGUAGE RecordWildCards #-}

module Emanote.Pandoc.Filter.Query (queryResolvingSplice, noteSplice) where

import Control.Lens.Operators ((^.))
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Map.Syntax ((##))
import qualified Data.Text as T
import qualified Ema
import Emanote.Model (Model)
import qualified Emanote.Model.Note as MN
import qualified Emanote.Model.Query as Q
import qualified Emanote.View.SiteRoute as SR
import qualified Heist as H
import qualified Heist.Extra.Splices.Pandoc as HP
import qualified Heist.Interpreted as HI
import qualified Heist.Splices.Json as HJ
import qualified Text.Pandoc.Definition as B
import qualified Text.XmlHtml as X

queryResolvingSplice :: Monad n => Model -> HP.RenderCtx n -> B.Block -> Maybe (HI.Splice n)
queryResolvingSplice model HP.RenderCtx {..} blk = do
  B.CodeBlock
    (_id', classes, _attrs)
    (Q.parseQuery -> Just q) <-
    pure blk
  guard $ List.elem "query" classes
  let mOtherCls = nonEmpty (List.delete "query" classes) <&> T.intercalate " " . toList
  -- TODO: This tag still remains in th>>e HTML; it should be removed.
  queryNode <- childElementTagWithClass "CodeBlock:Query" mOtherCls rootNode
  let splices = do
        "query"
          ## HI.textSplice (show q)
        "result"
          ## (HI.runChildrenWith . noteSplice model) `foldMapM` Q.runQuery model q
  pure $
    H.localHS (HI.bindSplices splices) $
      HI.runNode queryNode
  where
    childElementTagWithClass tag mCls node = do
      queryNodes <- nonEmpty $ X.childElementsTag tag node
      fmap head . nonEmpty $
        NE.filter ((== mCls) . X.getAttribute "class") queryNodes

-- TODO: Reuse this elsewhere
noteSplice :: Monad n => Model -> MN.Note -> H.Splices (HI.Splice n)
noteSplice model note = do
  "note:title" ## HI.textSplice (MN.noteTitle note)
  "note:url" ## HI.textSplice (Ema.routeUrl model $ SR.SRLMLFile $ note ^. MN.noteRoute)
  "note:metadata" ## HJ.bindJson (note ^. MN.noteMeta)
