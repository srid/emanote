module Emanote.View.QueryView (render) where

import Control.Lens.Operators ((^.))
import Data.IxSet.Typed qualified as Ix
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Map.Syntax ((##))
import Emanote.Model (Model)
import Emanote.Model.Meta qualified as Meta
import Emanote.Model.Query qualified as Query
import Emanote.Model.Task (Task)
import Emanote.Model.Task qualified as Task
import Emanote.Model.Type qualified as M
import Emanote.Pandoc.Renderer.Query (querySplice)
import Emanote.Route qualified as R
import Emanote.Route.SiteRoute qualified as SR
import Emanote.Route.SiteRoute.Class (indexLmlRoute)
import Emanote.View.Common qualified as Common
import Heist.Extra.Splices.List qualified as Splices
import Heist.Extra.Splices.Pandoc qualified as Splices
import Heist.Extra.Splices.Pandoc.Ctx (emptyRenderCtx)
import Heist.Interpreted qualified as HI
import Relude
import Text.Pandoc.Definition qualified as B

newtype TaskIndex = TaskIndex {unTaskIndex :: Map R.LMLRoute (NonEmpty Task)}

mkTaskIndex :: Model -> TaskIndex
mkTaskIndex model =
  TaskIndex . Map.map NE.sort $
    Map.fromListWith (<>) $
      filter (not . Task._taskChecked) (Ix.toList $ model ^. M.modelTasks) <&> \task ->
        (task ^. Task.taskRoute, one task)

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
