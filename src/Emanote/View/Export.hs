{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Emanote.View.Export (renderGraphExport) where

import Control.Lens ((^.))
import Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Emanote.Model (Model)
import qualified Emanote.Model as M
import qualified Emanote.Model.Link.Rel as Rel
import qualified Emanote.Model.Link.Resolve as Resolve
import qualified Emanote.Model.Title as Tit
import Emanote.Route (LMLRoute, lmlRouteCase)
import qualified Emanote.Route.R as R
import qualified Emanote.Route.SiteRoute as SR
import Emanote.Route.SiteRoute.Class (lmlSiteRoute)
import Relude

-- TODO: index.yaml and other per-route data?
-- TODO: Non-note files (static files)?
data Graph = Graph
  { version :: Int,
    description :: Text,
    vertices :: Map Text Vertex,
    edges :: Map Text [Link]
  }
  deriving (Generic, ToJSON)

data Vertex = Vertex
  { title :: Text,
    source :: Text,
    meta :: Aeson.Value
  }
  deriving (Generic, ToJSON)

data Link = Link
  { unresolvedRelTarget :: Rel.UnresolvedRelTarget,
    resolvedRelTarget :: Rel.ResolvedRelTarget Text
  }
  deriving (Generic, ToJSON)

renderGraphExport :: Model -> LByteString
renderGraphExport model =
  let notes_ =
        M.modelNoteMetas model & Map.mapKeys (lmlRouteUrl model)
          & Map.map
            ( \(tit, r, meta_) ->
                Vertex (Tit.toPlain tit) (toText $ lmlSourcePath r) meta_
            )
      rels_ =
        Map.fromListWith (<>) $
          M.modelNoteRels model <&> \rel ->
            let from_ = lmlRouteUrl model $ rel ^. Rel.relFrom
                to_ = rel ^. Rel.relTo
                toTarget =
                  Resolve.resolveUnresolvedRelTarget model to_
                    -- TODO: avoid query param, like in "favicon.svg?t=1633978084"
                    <&> SR.siteRouteUrl model
             in (from_, one $ Link to_ toTarget)
      description_ = "Emanote Graph of all files and links between them"
      export = Graph 1 description_ notes_ rels_
   in Aeson.encode export

-- URL of generated LML note
lmlRouteUrl :: Model -> LMLRoute -> Text
lmlRouteUrl model =
  SR.siteRouteUrl model . lmlSiteRoute

-- Path of the LML note
lmlSourcePath :: LMLRoute -> FilePath
lmlSourcePath =
  R.encodeRoute . lmlRouteCase