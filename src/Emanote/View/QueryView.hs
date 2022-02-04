module Emanote.View.QueryView (render) where

import Data.Map.Syntax ((##))
import Emanote.Model (Model)
import Emanote.Model.Meta qualified as Meta
import Emanote.Model.Query qualified as Query
import Emanote.Pandoc.Renderer.Query (querySplice)
import Emanote.Route.SiteRoute.Class (indexLmlRoute)
import Emanote.View.Common qualified as Common
import Heist.Extra.Splices.Pandoc.Ctx (emptyRenderCtx)
import Heist.Interpreted qualified as HI
import Relude

render :: Model -> Text -> LByteString
render model qs = do
  case Query.parseQuery qs of
    Nothing -> "Bad query: " <> encodeUtf8 qs
    Just q -> do
      let meta = Meta.getIndexYamlMeta model
          tCtx = Common.mkTemplateRenderCtx model indexLmlRoute meta

      Common.renderModelTemplate model "templates/special/queryview" $ do
        Common.commonSplices ($ emptyRenderCtx) model meta (fromString . toString $ qs <> " - Query View")
        "query:string" ## HI.textSplice qs
        querySplice (Common.withInlineCtx tCtx) indexLmlRoute model q
