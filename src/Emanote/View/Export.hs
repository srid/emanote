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
      ( R.encodeRoute $ R.lmlRouteCase $ rel ^. Rel.relFrom,
        Resolve.resolveUnresolvedRelTarget model (rel ^. Rel.relTo)
          <&> SR.siteRouteUrl model
      )
