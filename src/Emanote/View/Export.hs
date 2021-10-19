{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Emanote.View.Export (renderGraphExport) where

import Control.Lens ((^.))
import Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Emanote.Model (Model)
import qualified Emanote.Model as M
import qualified Emanote.Model.Graph as G
import qualified Emanote.Model.Link.Rel as Rel
import qualified Emanote.Model.Link.Resolve as Resolve
import qualified Emanote.Model.Title as Tit
import Emanote.Route (LMLRoute, lmlRouteCase)
import qualified Emanote.Route as R
import qualified Emanote.Route.SiteRoute as SR
import Emanote.Route.SiteRoute.Class (lmlSiteRoute)
import Relude

data Export = Export
  { version :: Word,
    linkGraph :: Graph,
    urls :: Map Text Text
  }
  deriving (Generic, ToJSON)

currentVersion :: Word
currentVersion = 1

data Graph = Graph
  { description :: Text,
    vertices :: Map Text Vertex,
    edges :: Map Text [Link]
  }
  deriving (Generic, ToJSON)

data Vertex = Vertex
  { title :: Text,
    source :: Text,
    parent :: Maybe Text,
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
        M.modelNoteMetas model & Map.mapKeys lmlRouteKey
          & Map.map
            ( \(tit, r, meta_) ->
                Vertex
                  (Tit.toPlain tit)
                  (toText $ lmlSourcePath r)
                  (toText . lmlSourcePath <$> G.parentLmlRoute r)
                  meta_
            )
      rels_ =
        Map.fromListWith (<>) $
          M.modelNoteRels model <&> \rel ->
            let from_ = lmlRouteKey $ rel ^. Rel.relFrom
                to_ = rel ^. Rel.relTo
                toTarget =
                  Resolve.resolveUnresolvedRelTarget model to_
                    <&> SR.siteRouteUrlStatic model
             in (from_, one $ Link to_ toTarget)
      description_ = "Emanote Graph of all files and links between them"
      graph_ = Graph description_ notes_ rels_
      urls_ =
        Map.fromList $ M.modelNoteRoutes model <&> \r -> (lmlRouteKey r, SR.siteRouteUrl model $ lmlSiteRoute r)
      export = Export currentVersion graph_ urls_
   in Aeson.encode export

-- An unique key to represent this LMLRoute in the exported JSON
--
-- We use the source path consistently.
lmlRouteKey :: LMLRoute -> Text
lmlRouteKey =
  toText . R.encodeRoute . R.lmlRouteCase

-- Path of the LML note
lmlSourcePath :: LMLRoute -> FilePath
lmlSourcePath =
  R.encodeRoute . lmlRouteCase