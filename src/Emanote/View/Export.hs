{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Emanote.View.Export (renderExport) where

import Control.Lens ((^.))
import Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Emanote.Model (Model)
import qualified Emanote.Model as M
import qualified Emanote.Model.Link.Rel as Rel
import qualified Emanote.Model.Link.Resolve as Resolve
import qualified Emanote.Route.SiteRoute as SR
import Emanote.Route.SiteRoute.Class (lmlSiteRoute)
import Relude

data Export = Export
  { version :: Int,
    rels :: Map Text [Link]
  }
  deriving (Generic, ToJSON)

data Link = Link
  { unresolvedRelTarget :: Rel.UnresolvedRelTarget,
    resolvedRelTarget :: Rel.ResolvedRelTarget Text
  }
  deriving (Generic, ToJSON)

renderExport :: Model -> LByteString
renderExport model =
  let rels_ =
        Map.fromListWith (<>) $
          M.modelNoteRels model <&> \rel ->
            let from_ = SR.siteRouteUrl model $ lmlSiteRoute $ rel ^. Rel.relFrom
                to_ = rel ^. Rel.relTo
                toTarget =
                  Resolve.resolveUnresolvedRelTarget model to_
                    <&> SR.siteRouteUrl model
             in (from_, one $ Link to_ toTarget)
      export = Export 1 rels_
   in Aeson.encode export
