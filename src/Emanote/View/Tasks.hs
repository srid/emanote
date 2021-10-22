{-# LANGUAGE TypeApplications #-}

module Emanote.View.Tasks (renderTasks) where

import Emanote.Model (Model)
import qualified Emanote.Model.Meta as Meta
import Emanote.View.Common (commonSplices, renderModelTemplate)
import Heist.Extra.Splices.Pandoc.Ctx (emptyRenderCtx)
import Relude

renderTasks :: Model -> LByteString
renderTasks model = do
  let meta = Meta.getIndexYamlMeta model
  renderModelTemplate model "templates/special/tasks" $ do
    commonSplices ($ emptyRenderCtx) model meta "Task Tracker"
