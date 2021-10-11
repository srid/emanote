module Emanote.View.Export where

import Control.Lens ((^.))
import qualified Data.Aeson as Aeson
import Emanote.Model (Model)
import qualified Emanote.Model as M
import qualified Emanote.Model.Link.Rel as Rel
import qualified Emanote.Model.Link.Resolve as Resolve
import qualified Emanote.Route as R
import qualified Emanote.Route.SiteRoute as SR
import Relude

renderExport :: Model -> LByteString
renderExport model =
  Aeson.encode $
    M.modelNoteRels model <&> \rel ->
      let from_ = R.encodeRoute $ R.lmlRouteCase $ rel ^. Rel.relFrom
          to = rel ^. Rel.relTo
          toTarget =
            Resolve.resolveUnresolvedRelTarget model to
              <&> SR.siteRouteUrl model
       in (from_, (to, toTarget))
