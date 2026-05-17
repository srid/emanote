module Emanote.Model.GraphSpec where

import Emanote.Model.Graph (modelLookupBacklinks)
import Emanote.Model.Note qualified as MN
import Emanote.Model.Type qualified as M
import Emanote.Route.ModelRoute (LMLRoute (LMLRoute_Md))
import Emanote.Route.R (R (..))
import Relude
import Test.Hspec
import Text.Pandoc.Definition qualified as B

spec :: Spec
spec = do
  describe "modelLookupBacklinks" $ do
    it "includes extensionless Markdown links as backlinks" $ do
      let sourceRoute = LMLRoute_Md $ R ("index" :| [])
          targetRoute = LMLRoute_Md $ R ("guide" :| ["neuron"])
          sourceNote =
            MN.mkEmptyNoteWith
              sourceRoute
              [ B.Para
                  [ B.Str "See "
                  , B.Link B.nullAttr [B.Str "Neuron"] ("guide/neuron", "")
                  ]
              ]
          targetNote = MN.mkEmptyNoteWith targetRoute []
          model =
            M.withRoutePrism (error "route prism unused by modelLookupBacklinks")
              $ M.modelInsertNote sourceNote
              $ M.modelInsertNote targetNote
              $ M.emptyModel
                mempty
                (error "CLI action unused by modelLookupBacklinks")
                (error "renderers unused by modelLookupBacklinks")
                (error "scripting engine unused by modelLookupBacklinks")
                M.ModelFlags
                  { M.modelFlagCompileTailwind = False
                  , M.modelFlagAllowBrokenLuaFilters = False
                  }
                (error "instance ID unused by modelLookupBacklinks")
                (error "stork index unused by modelLookupBacklinks")

      fmap fst (modelLookupBacklinks targetRoute model) `shouldBe` [sourceRoute]
