module Emanote.View.QueryView (render) where

import Data.Map.Syntax ((##))
import Emanote.Model (Model)
import Emanote.Model.Meta qualified as Meta
import Emanote.Model.Query.Type (Query)
import Emanote.Pandoc.Renderer.Query (querySplice)
import Emanote.Route.SiteRoute.Class (indexLmlRoute)
import Emanote.View.Common qualified as Common
import Heist.Extra.Splices.Pandoc.Ctx (emptyRenderCtx)
import Heist.Interpreted qualified as HI
import Relude

render :: Model -> Query -> LByteString
render model q = do
  let meta = Meta.getIndexYamlMeta model
      tCtx = Common.mkTemplateRenderCtx model indexLmlRoute meta

  Common.renderModelTemplate model "templates/special/queryview" $ do
    Common.commonSplices ($ emptyRenderCtx) model meta (show q <> " - Query View")
    "query:string" ## HI.textSplice (show q)
    querySplice (Common.withInlineCtx tCtx) indexLmlRoute model q
