module Emanote.Model.SourceDependenciesSpec where

import Data.Set qualified as Set
import Emanote.Model.SourceDependencies qualified as SDeps
import Emanote.Route qualified as R
import Relude
import Test.Hspec

mdRoute :: FilePath -> R.LMLRoute
mdRoute fp =
  fromMaybe (error $ "bad test route: " <> toText fp)
    $ R.mkLMLRouteFromKnownFilePath R.Md fp

spec :: Spec
spec = do
  let n1 = mdRoute "a.md"
      n2 = mdRoute "b.md"
      n3 = mdRoute "c.md"
      filterX = "/notebook/filters/x.lua"
      filterY = "/notebook/filters/y.lua"
  describe "setLuaDeps" $ do
    it "registers a single edge" $ do
      let sd = SDeps.setLuaDeps n1 [filterX] SDeps.emptyDependencies
      SDeps.dependentsOnLua filterX sd `shouldBe` one n1
    it "groups multiple notes under the same filter" $ do
      let sd =
            SDeps.setLuaDeps n2 [filterX]
              $ SDeps.setLuaDeps n1 [filterX] SDeps.emptyDependencies
      SDeps.dependentsOnLua filterX sd `shouldBe` Set.fromList [n1, n2]
    it "splits a note across multiple filters" $ do
      let sd = SDeps.setLuaDeps n1 [filterX, filterY] SDeps.emptyDependencies
      SDeps.dependentsOnLua filterX sd `shouldBe` one n1
      SDeps.dependentsOnLua filterY sd `shouldBe` one n1
    it "drops stale edges when a note's filter list shrinks" $ do
      let sd1 = SDeps.setLuaDeps n1 [filterX, filterY] SDeps.emptyDependencies
          sd2 = SDeps.setLuaDeps n1 [filterX] sd1
      SDeps.dependentsOnLua filterX sd2 `shouldBe` one n1
      SDeps.dependentsOnLua filterY sd2 `shouldBe` mempty
    it "leaves co-tenants intact when one note's edges change" $ do
      let sd1 =
            SDeps.setLuaDeps n2 [filterX]
              $ SDeps.setLuaDeps n1 [filterX] SDeps.emptyDependencies
          sd2 = SDeps.setLuaDeps n1 [] sd1
      SDeps.dependentsOnLua filterX sd2 `shouldBe` one n2
  describe "removeNote" $ do
    it "drops every edge originating at the removed note" $ do
      let sd1 = SDeps.setLuaDeps n1 [filterX, filterY] SDeps.emptyDependencies
          sd2 = SDeps.removeNote n1 sd1
      SDeps.dependentsOnLua filterX sd2 `shouldBe` mempty
      SDeps.dependentsOnLua filterY sd2 `shouldBe` mempty
    it "spares unrelated notes" $ do
      let sd1 =
            SDeps.setLuaDeps n3 [filterY]
              $ SDeps.setLuaDeps n2 [filterX]
              $ SDeps.setLuaDeps n1 [filterX] SDeps.emptyDependencies
          sd2 = SDeps.removeNote n1 sd1
      SDeps.dependentsOnLua filterX sd2 `shouldBe` one n2
      SDeps.dependentsOnLua filterY sd2 `shouldBe` one n3
  describe "dependentsOnLua" $ do
    it "returns empty for an unknown filter path"
      $ SDeps.dependentsOnLua filterX SDeps.emptyDependencies
      `shouldBe` mempty
