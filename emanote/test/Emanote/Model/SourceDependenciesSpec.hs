module Emanote.Model.SourceDependenciesSpec where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Emanote.Model.SourceDependencies qualified as SDeps
import Emanote.Route qualified as R
import Emanote.Source.Loc (Loc (..))
import Relude
import Test.Hspec

mdRoute :: FilePath -> R.LMLRoute
mdRoute fp =
  fromMaybe (error $ "bad test route: " <> toText fp)
    $ R.mkLMLRouteFromKnownFilePath R.Md fp

{- | Synthetic source location for tests; the @(Loc, FilePath)@ is
carried on every dep edge but its content is opaque to the index.
-}
src :: FilePath -> (Loc, FilePath)
src fp = (LocUser 1 "/test" Nothing, fp)

routes :: SDeps.SourceDependencies -> FilePath -> Set R.LMLRoute
routes sd fp = Map.keysSet (SDeps.dependentsOnLua fp sd)

spec :: Spec
spec = do
  let n1 = mdRoute "a.md"
      n2 = mdRoute "b.md"
      n3 = mdRoute "c.md"
      -- Keys are filter paths in the form they appear in
      -- @pandoc.filters.*@ frontmatter — typically layer-relative,
      -- not resolved absolute paths.
      filterX = "filters/x.lua"
      filterY = "filters/y.lua"
      filterRender = "filters/r.lua"
      missing = "filters/not-yet-on-disk.lua"
      setParse note s ps = SDeps.setLuaDeps note s ps []
  describe "setLuaDeps" $ do
    it "registers a single edge" $ do
      let sd = setParse n1 (src "a.md") [filterX] SDeps.emptyDependencies
      routes sd filterX `shouldBe` one n1
    it "groups multiple notes under the same filter" $ do
      let sd =
            setParse n2 (src "b.md") [filterX]
              $ setParse n1 (src "a.md") [filterX] SDeps.emptyDependencies
      routes sd filterX `shouldBe` Set.fromList [n1, n2]
    it "splits a note across multiple filters" $ do
      let sd = setParse n1 (src "a.md") [filterX, filterY] SDeps.emptyDependencies
      routes sd filterX `shouldBe` one n1
      routes sd filterY `shouldBe` one n1
    it "drops stale edges when a note's filter list shrinks" $ do
      let sd1 = setParse n1 (src "a.md") [filterX, filterY] SDeps.emptyDependencies
          sd2 = setParse n1 (src "a.md") [filterX] sd1
      routes sd2 filterX `shouldBe` one n1
      routes sd2 filterY `shouldBe` mempty
    it "leaves co-tenants intact when one note's edges change" $ do
      let sd1 =
            setParse n2 (src "b.md") [filterX]
              $ setParse n1 (src "a.md") [filterX] SDeps.emptyDependencies
          sd2 = setParse n1 (src "a.md") [] sd1
      routes sd2 filterX `shouldBe` one n2
    it "registers edges for filters that were unresolved at parse time" $ do
      -- The dep index doesn't care whether the path resolves on disk.
      -- This is what makes "filter created later" trigger a re-parse:
      -- the same key matches when the LuaFilter handler fires.
      let sd = setParse n1 (src "a.md") [missing] SDeps.emptyDependencies
      routes sd missing `shouldBe` one n1
    it "stores the dependent's source location on the edge" $ do
      -- The (Loc, FilePath) is what 'parseAndInsert' needs to re-read
      -- the dependent on a Lua filter change — without consulting the
      -- model. This is the load-bearing property the new shape adds.
      let sd = setParse n1 (src "a.md") [filterX] SDeps.emptyDependencies
      Map.lookup n1 (SDeps.dependentsOnLua filterX sd) `shouldBe` Just (src "a.md")
    it "overwrites the source location when the same note is re-registered" $ do
      let sd1 = setParse n1 (src "old.md") [filterX] SDeps.emptyDependencies
          sd2 = setParse n1 (src "new.md") [filterX] sd1
      Map.lookup n1 (SDeps.dependentsOnLua filterX sd2) `shouldBe` Just (src "new.md")
    it "tracks render-phase deps alongside parse-phase deps" $ do
      -- A note can declare a parse-time filter and a render-time filter
      -- simultaneously; both are reachable through dependentsOnLua so
      -- the patcher invalidates the note when either file changes.
      let sd = SDeps.setLuaDeps n1 (src "a.md") [filterX] [filterRender] SDeps.emptyDependencies
      routes sd filterX `shouldBe` one n1
      routes sd filterRender `shouldBe` one n1
  describe "removeNote" $ do
    it "drops every edge originating at the removed note" $ do
      let sd1 = SDeps.setLuaDeps n1 (src "a.md") [filterX, filterY] [filterRender] SDeps.emptyDependencies
          sd2 = SDeps.removeNote n1 sd1
      routes sd2 filterX `shouldBe` mempty
      routes sd2 filterY `shouldBe` mempty
      routes sd2 filterRender `shouldBe` mempty
    it "spares unrelated notes" $ do
      let sd1 =
            setParse n3 (src "c.md") [filterY]
              $ setParse n2 (src "b.md") [filterX]
              $ setParse n1 (src "a.md") [filterX] SDeps.emptyDependencies
          sd2 = SDeps.removeNote n1 sd1
      routes sd2 filterX `shouldBe` one n2
      routes sd2 filterY `shouldBe` one n3
  describe "dependentsOnLua" $ do
    it "returns empty for an unknown filter path"
      $ SDeps.dependentsOnLua filterX SDeps.emptyDependencies
      `shouldBe` mempty
