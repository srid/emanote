module Emanote.Route.RSpec where

import Emanote.Route.Ext
import Emanote.Route.R
import Hedgehog
import Relude
import Test.Hspec
import Test.Hspec.Hedgehog

type SomeExt = ('LMLType 'Md)

spec :: Spec
spec = do
  mkRouteFromFilePathSpec
  routeInitsSpec

mkRouteFromFilePathSpec :: Spec
mkRouteFromFilePathSpec = describe "mkRouteFromFilePath" $ do
  describe "basic" $ do
    it "index route" . hedgehog $ do
      mkRouteFromFilePath @_ @SomeExt "index.md" === Just indexRoute
    it "single slug" . hedgehog $ do
      mkRouteFromFilePath "foo.md" === Just r1
    it "two slugs" . hedgehog $ do
      mkRouteFromFilePath "foo/bar.md" === Just r2
    it "three slugs" . hedgehog $ do
      mkRouteFromFilePath "foo/bar/qux.md" === Just r3

routeInitsSpec :: Spec
routeInitsSpec = describe "routeInits" $ do
  describe "basic" $ do
    it "index route returns itself" . hedgehog $ do
      routeInits rIndex === one rIndex
    it "single slug returns index and itself" . hedgehog $ do
      routeInits r1 === rIndex :| [r1]
    it "two slugs returns index, first slug, and itself" . hedgehog $ do
      routeInits r2 === rIndex :| [r1, r2]
    it "three slugs returns index, first slug, second slug, and itself" . hedgehog $ do
      routeInits r3 === rIndex :| [r1, r2, r3]

r1 :: R ('LMLType 'Md)
r1 = R $ "foo" :| []

r1Index :: R ('LMLType 'Md)
r1Index = R $ "foo" :| ["index"]

r2 :: R ('LMLType 'Md)
r2 = R $ "foo" :| ["bar"]

r2Index :: R ('LMLType 'Md)
r2Index = R $ "foo" :| ["bar", "index"]

r3 :: R ('LMLType 'Md)
r3 = R $ "foo" :| ["bar", "qux"]

r3Index :: R ('LMLType 'Md)
r3Index = R $ "foo" :| ["bar", "qux", "index"]

rIndex :: R ('LMLType 'Md)
rIndex = R $ "index" :| []
