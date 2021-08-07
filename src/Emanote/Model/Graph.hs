module Emanote.Model.Graph
  ( Graph,
    empty,
    module P,
  )
where

import qualified Algebra.Graph.Labelled.AdjacencyMap as AM
import Algebra.Graph.Labelled.AdjacencyMap.Patch as P
  ( ModifyGraph (..),
  )
import qualified Emanote.Pandoc.Markdown.Syntax.WikiLink as WL
import qualified Emanote.Route as R
import Prelude hiding (empty)

type Graph = AM.AdjacencyMap (Maybe WL.WikiLinkType) R.LMLRoute

empty :: Graph
empty = AM.empty
