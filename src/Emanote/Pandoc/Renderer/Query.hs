module Emanote.Pandoc.Renderer.Query
  ( queryResolvingSplice,
    noteSpliceMap,
  )
where

import Data.List qualified as List
import Data.Map.Syntax ((##))
import Data.Text qualified as T
import Emanote.Model (Model)
import Emanote.Model.Note qualified as MN
import Emanote.Model.Query qualified as Q
import Emanote.Model.Title qualified as Tit
import Emanote.Pandoc.BuiltinFilters (preparePandoc)
import Emanote.Pandoc.Renderer (PandocBlockRenderer)
import Emanote.Route (LMLRoute)
import Emanote.Route.SiteRoute qualified as SR
import Heist qualified as H
import Heist.Extra qualified as HE
import Heist.Extra.Splices.Pandoc (RenderCtx)
import Heist.Interpreted qualified as HI
import Heist.Splices.Json qualified as HJ
import Optics.Operators ((^.))
import Relude
import Text.Pandoc.Definition qualified as B

queryResolvingSplice :: forall n i. Monad n => PandocBlockRenderer n i LMLRoute
queryResolvingSplice model _nr ctx noteRoute blk = do
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
  "ema:note:title" ## withCtx $ \ctx -> Tit.titleSplice ctx preparePandoc (MN._noteTitle note)
  "ema:note:url" ## HI.textSplice (SR.siteRouteUrl model $ SR.lmlSiteRoute $ note ^. MN.noteRoute)
  "ema:note:metadata" ## HJ.bindJson (note ^. MN.noteMeta)
