{-# LANGUAGE TypeApplications #-}

module Emanote.View.Tasks (renderTasks) where

import Control.Lens.Operators ((^.))
import qualified Data.IxSet.Typed as Ix
import Data.Map.Syntax ((##))
import Emanote.Model (Model)
import qualified Emanote.Model.Meta as Meta
import qualified Emanote.Model.Task as Task
import qualified Emanote.Model.Title as Title
import qualified Emanote.Model.Type as M
import Emanote.Pandoc.BuiltinFilters (preparePandoc)
import qualified Emanote.Route.SiteRoute as SR
import Emanote.View.Common (commonSplices, inlineRenderers, linkInlineRenderers, mkRendererFromMeta, renderModelTemplate)
import qualified Heist.Extra.Splices.List as Splices
import qualified Heist.Extra.Splices.Pandoc as Splices
import Heist.Extra.Splices.Pandoc.Ctx (emptyRenderCtx)
import qualified Heist.Interpreted as HI
import Relude
import qualified Text.Pandoc.Definition as B

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
      tasks = Ix.toList $ model ^. M.modelTasks
      taskSplice task = do
        let r = task ^. Task.taskRoute
        -- TODO: reuse note splice
        "task:description" ## withInlineCtx $ \ctx ->
          Splices.pandocSplice ctx $ B.Pandoc mempty $ one $ B.Plain $ task ^. Task.taskDescription
        "note:title" ## titleSplice (M.modelLookupTitle r model)
        "note:url" ## HI.textSplice (SR.siteRouteUrl model $ SR.lmlSiteRoute r)

  renderModelTemplate model "templates/special/tasks" $ do
    commonSplices ($ emptyRenderCtx) model meta "Task Tracker"
    "ema:tasks"
      ## Splices.listSplice tasks "task" taskSplice
