{-# LANGUAGE RecordWildCards #-}

module Emanote.Pandoc.Renderer.Query
  ( queryResolvingSplice,
    noteSpliceMap,
  )
where

import Control.Lens.Operators ((^.))
import qualified Data.List as List
import Data.Map.Syntax ((##))
import qualified Data.Text as T
import Emanote.Model (Model)
import qualified Emanote.Model.Note as MN
import qualified Emanote.Model.Query as Q
import qualified Emanote.Model.Title as Tit
import Emanote.Pandoc.BuiltinFilters (preparePandoc)
import Emanote.Pandoc.Renderer (PandocBlockRenderer)
import Emanote.Route (LMLRoute)
import qualified Emanote.Route.SiteRoute as SR
import qualified Heist as H
import qualified Heist.Extra as HE
import Heist.Extra.Splices.Pandoc (RenderCtx)
import qualified Heist.Interpreted as HI
import qualified Heist.Splices.Json as HJ
import qualified Text.Pandoc.Definition as B

queryResolvingSplice :: forall n i b. Monad n => PandocBlockRenderer n i b LMLRoute
queryResolvingSplice _emaAction model _nr ctx noteRoute blk = do
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
        ## (HI.runChildrenWith . noteSpliceMap ($ ctx) model) `foldMapM` Q.runQuery noteRoute model q

-- TODO: Reuse this elsewhere
noteSpliceMap ::
  Monad n =>
  ((RenderCtx n -> HI.Splice n) -> HI.Splice n) ->
  Model ->
  MN.Note ->
  H.Splices (HI.Splice n)
noteSpliceMap withCtx model note = do
  "ema:note:title" ## withCtx $ \ctx -> Tit.titleSplice ctx (preparePandoc model) (MN._noteTitle note)
  "ema:note:url" ## HI.textSplice (SR.siteRouteUrl model $ SR.lmlSiteRoute $ note ^. MN.noteRoute)
  "ema:note:metadata" ## HJ.bindJson (note ^. MN.noteMeta)
