module Emanote.View.TaskIndex (renderTasks) where

import Data.IxSet.Typed qualified as Ix
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Map.Syntax ((##))
import Emanote.Model (Model)
import Emanote.Model.Task (Task)
import Emanote.Model.Task qualified as Task
import Emanote.Model.Type qualified as M
import Emanote.Route qualified as R
import Emanote.Route.SiteRoute qualified as SR
import Emanote.View.Common qualified as Common
import Heist.Extra.Splices.List qualified as Splices
import Heist.Extra.Splices.Pandoc qualified as Splices
import Heist.Extra.Splices.Pandoc.Ctx (emptyRenderCtx)
import Heist.Interpreted qualified as HI
import Optics.Operators ((^.))
import Relude
import Text.Pandoc.Definition qualified as B

newtype TaskIndex = TaskIndex {unTaskIndex :: Map R.LMLRoute (NonEmpty Task)}

mkTaskIndex :: Model -> TaskIndex
mkTaskIndex model =
  TaskIndex . Map.map NE.sort $
    Map.fromListWith (<>) $
      filter (not . Task._taskChecked) (Ix.toList $ model ^. M.modelTasks) <&> \task ->
        (task ^. Task.taskRoute, one task)

renderTasks :: Model -> LByteString
renderTasks model = do
  let (defR, meta) = Common.defaultRouteMeta model
      tCtx = Common.mkTemplateRenderCtx model defR meta
      taskIndex = mkTaskIndex model
      taskGroupSplice r tasks = do
        "t:note:url" ## HI.textSplice (SR.siteRouteUrl model $ SR.lmlSiteRoute r)
        "t:note:title" ## Common.titleSplice tCtx (M.modelLookupTitle r model)
        "t:note:breadcrumbs" ##
          Common.routeBreadcrumbs tCtx model r
        "t:tasks" ## Splices.listSplice (toList tasks) "task" taskSplice
      taskSplice task = do
        let r = task ^. Task.taskRoute
        -- TODO: reuse note splice
        "task:description" ## Common.withInlineCtx tCtx $ \ctx ->
          Splices.pandocSplice ctx $ B.Pandoc mempty $ one $ B.Plain $ task ^. Task.taskDescription
        "note:title" ## Common.titleSplice tCtx (M.modelLookupTitle r model)
        "note:url" ## HI.textSplice (SR.siteRouteUrl model $ SR.lmlSiteRoute r)

  Common.renderModelTemplate model "templates/special/tasks" $ do
    Common.commonSplices ($ emptyRenderCtx) model meta "Task Index"
    let groups =
          Map.toList (unTaskIndex taskIndex)
            & sortWith fst
    "ema:taskGroups" ## Splices.listSplice groups "taskGroup" (uncurry taskGroupSplice)
