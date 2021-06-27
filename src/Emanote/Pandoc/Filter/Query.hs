{-# LANGUAGE RecordWildCards #-}

module Emanote.Pandoc.Filter.Query (queryResolvingSplice, noteSplice) where

import Control.Lens.Operators ((^.))
import qualified Data.List as List
import Data.Map.Syntax ((##))
import qualified Data.Text as T
import qualified Ema
import Emanote.Model (Model)
import qualified Emanote.Model.Note as MN
import qualified Emanote.Model.Query as Q
import qualified Emanote.Model.Title as Tit
import qualified Emanote.Route.SiteRoute as SR
import qualified Heist as H
import qualified Heist.Extra as HE
import qualified Heist.Extra.Splices.Pandoc as HP
import qualified Heist.Interpreted as HI
import qualified Heist.Splices.Json as HJ
import qualified Text.Pandoc.Definition as B

queryResolvingSplice :: Monad n => MN.Note -> Model -> HP.RenderCtx n -> B.Block -> Maybe (HI.Splice n)
queryResolvingSplice currentNote model _ctx blk = do
  B.CodeBlock
    (_id', classes, _attrs)
    (Q.parseQuery -> Just q) <-
    pure blk
  guard $ List.elem "query" classes
  let mOtherCls = nonEmpty (List.delete "query" classes) <&> T.intercalate " " . toList
      queryTpl = encodeUtf8 $ "/templates/filters/query-" <> fromMaybe "default" mOtherCls
  pure $ do
    tpl <- HE.lookupHtmlTemplateMust queryTpl
    HE.runCustomTemplate tpl $ do
      "query"
        ## HI.textSplice (show q)
      "result"
        ## (HI.runChildrenWith . noteSplice model) `foldMapM` Q.runQuery currentNote model q

-- TODO: Reuse this elsewhere
noteSplice :: Monad n => Model -> MN.Note -> H.Splices (HI.Splice n)
noteSplice model note = do
  "ema:note:title" ## Tit.titleSplice (MN._noteTitle note)
  "ema:note:url" ## HI.textSplice (SR.siteRouteUrl model $ SR.lmlSiteRoute $ note ^. MN.noteRoute)
  "ema:note:metadata" ## HJ.bindJson (note ^. MN.noteMeta)
