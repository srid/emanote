{-# LANGUAGE TypeApplications #-}

module Emanote.View.TaskIndex (renderTasks) where

import Control.Lens.Operators ((^.))
import qualified Data.IxSet.Typed as Ix
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Map.Syntax ((##))
import Emanote.Model (Model)
import qualified Emanote.Model.Meta as Meta
import Emanote.Model.Task (Task)
import qualified Emanote.Model.Task as Task
import qualified Emanote.Model.Title as Title
import qualified Emanote.Model.Type as M
import Emanote.Pandoc.BuiltinFilters (preparePandoc)
import qualified Emanote.Route as R
import qualified Emanote.Route.SiteRoute as SR
import Emanote.View.Common (commonSplices, inlineRenderers, linkInlineRenderers, mkRendererFromMeta, renderModelTemplate)
import qualified Heist.Extra.Splices.List as Splices
import qualified Heist.Extra.Splices.Pandoc as Splices
import Heist.Extra.Splices.Pandoc.Ctx (emptyRenderCtx)
import qualified Heist.Interpreted as HI
import Relude
import qualified Text.Pandoc.Definition as B

newtype TaskIndex = TaskIndex {unTaskIndex :: Map R.LMLRoute (NonEmpty Task)}

mkTaskIndex :: Model -> TaskIndex
mkTaskIndex model =
  TaskIndex . Map.map NE.sort $
    Map.fromListWith (<>) $
      filter (not . Task._taskChecked) (Ix.toList $ model ^. M.modelTasks) <&> \task ->
        (task ^. Task.taskRoute, one task)

renderTasks :: Model -> LByteString
renderTasks model = do
  let meta = Meta.getIndexYamlMeta model
      -- TODO: Refactor with Template.hs for DRY
      withNoteRenderer = mkRendererFromMeta model meta
      withInlineCtx =
        withNoteRenderer inlineRenderers () ()
      withLinkInlineCtx =
        withNoteRenderer linkInlineRenderers () ()
      titleSplice titleDoc = withLinkInlineCtx $ \x ->
        Title.titleSplice x (preparePandoc model) titleDoc
      taskIndex = mkTaskIndex model
      taskGroupSplice r tasks = do
        "t:note:url" ## HI.textSplice (SR.siteRouteUrl model $ SR.lmlSiteRoute r)
        "t:note:title" ## titleSplice (M.modelLookupTitle r model)
        "t:tasks" ## Splices.listSplice (toList tasks) "task" taskSplice
      taskSplice task = do
        let r = task ^. Task.taskRoute
        -- TODO: reuse note splice
        "task:description" ## withInlineCtx $ \ctx ->
          Splices.pandocSplice ctx $ B.Pandoc mempty $ one $ B.Plain $ task ^. Task.taskDescription
        "note:title" ## titleSplice (M.modelLookupTitle r model)
        "note:url" ## HI.textSplice (SR.siteRouteUrl model $ SR.lmlSiteRoute r)

  renderModelTemplate model "templates/special/tasks" $ do
    commonSplices ($ emptyRenderCtx) model meta "Task Index"
    let groups =
          Map.toList (unTaskIndex taskIndex)
            & sortOn fst
    "ema:taskGroups" ## Splices.listSplice groups "taskGroup" (uncurry taskGroupSplice)
