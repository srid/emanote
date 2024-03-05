module Emanote.Model.TocSpec where

import Control.Monad.Writer (runWriterT)
import Data.Tree
import Emanote.Model.Note (parseNoteOrg)
import Emanote.Model.Toc
import Relude
import Test.Hspec

spec :: Spec
spec = do
  describe "basic-toc" $ do
    it "create toc tree" $ do
      ((doc, _), []) <- runWriterT $ parseNoteOrg demo
      (fmap headingName <$> newToc doc)
        `shouldBe` [ Node
                      { rootLabel = "h1 1"
                      , subForest =
                          [ Node {rootLabel = "h2 1", subForest = []}
                          , Node {rootLabel = "h2 2", subForest = []}
                          ]
                      }
                   , Node {rootLabel = "h1 2", subForest = []}
                   ]
  where
    demo =
      unlines
        [ "* h1  1"
        , "** h2 1"
        , "** h2 2"
        , "* h1  2"
        , -- this is ignored because of missing h2 heading
          "*** h3 1"
        ]
