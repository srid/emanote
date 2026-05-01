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
  expandIndexSlugSpec

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
  describe "dropIndex" $ do
    it "index route" . hedgehog $ do
      mkRouteFromFilePath' True "index.md" === Just rIndex
    it "single slug" . hedgehog $ do
      mkRouteFromFilePath' True "foo.md" === Just r1
    it "two slugs" . hedgehog $ do
      mkRouteFromFilePath' True "foo/index.md" === Just r1
    it "three slugs" . hedgehog $ do
      mkRouteFromFilePath' True "foo/bar/index.md" === Just r2
    it "nested index folder file (#542)" . hedgehog $ do
      -- foo/index/index.md keeps the folder slug; only the trailing
      -- filename "index" is dropped.
      mkRouteFromFilePath' True "foo/index/index.md" === Just r1Index
    it "deeply nested index folders (#542)" . hedgehog $ do
      mkRouteFromFilePath' True "index/index/index/example.md" === Just rNested

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

expandIndexSlugSpec :: Spec
expandIndexSlugSpec = describe "expandIndexSlug" $ do
  it "leaves the lone index slug alone" . hedgehog $ do
    expandIndexSlug ("index" :| []) === "index" :| []
  it "leaves a single non-index slug alone" . hedgehog $ do
    expandIndexSlug ("foo" :| []) === "foo" :| []
  it "leaves multi-slug routes ending in non-index alone" . hedgehog $ do
    expandIndexSlug ("foo" :| ["bar"]) === "foo" :| ["bar"]
  it "appends an index slug when a multi-slug route ends in index (#542)" . hedgehog $ do
    -- LML route for `foo/index/index.md` is ("foo","index"); its HTML route
    -- must encode to `foo/index/index.html` so pretty URL yields /foo/index/.
    expandIndexSlug ("foo" :| ["index"]) === "foo" :| ["index", "index"]
  it "appends index for nested index folders (#542)" . hedgehog $ do
    -- Folder placeholder for index/index/index/.
    expandIndexSlug ("index" :| ["index", "index"])
      === "index" :| ["index", "index", "index"]

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

-- Deeply nested index folders, exercising #542.
rNested :: R ('LMLType 'Md)
rNested = R $ "index" :| ["index", "index", "example"]
