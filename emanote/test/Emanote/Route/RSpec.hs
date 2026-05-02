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
  folderNoteCoexistenceSpec

mkRouteFromFilePathSpec :: Spec
mkRouteFromFilePathSpec = do
  describe "mkRouteFromFilePath" $ do
    it "index route" . hedgehog $ do
      mkRouteFromFilePath @_ @SomeExt "index.md" === Just indexRoute
    it "single slug" . hedgehog $ do
      mkRouteFromFilePath "foo.md" === Just r1
    it "two slugs" . hedgehog $ do
      mkRouteFromFilePath "foo/bar.md" === Just r2
    it "three slugs" . hedgehog $ do
      mkRouteFromFilePath "foo/bar/qux.md" === Just r3
  describe "mkLmlRouteFromFilePath" $ do
    it "index route" . hedgehog $ do
      mkLmlRouteFromFilePath "index.md" === Just rIndex
    it "single slug" . hedgehog $ do
      mkLmlRouteFromFilePath "foo.md" === Just r1
    it "two slugs" . hedgehog $ do
      mkLmlRouteFromFilePath "foo/index.md" === Just r1
    it "three slugs" . hedgehog $ do
      mkLmlRouteFromFilePath "foo/bar/index.md" === Just r2
    it "nested index folder file (#542)" . hedgehog $ do
      -- foo/index/index.md keeps the folder slug; only the trailing
      -- filename "index" is dropped.
      mkLmlRouteFromFilePath "foo/index/index.md" === Just r1Index
    it "deeply nested index folders (#542)" . hedgehog $ do
      mkLmlRouteFromFilePath "index/index/index/example.md" === Just rNested

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

-- Regression suite for the coexistence of @foo/index.md@ (a folder note for
-- @foo\/@) with deeply-nested index-named folders like
-- @foo\/index\/{index.md,bar.md}@. Pre-#542 these collided onto one URL; the
-- fix sends them to distinct HTML routes. The cases below pin every link in
-- that chain — file path → LML route → HTML route slugs — so a future
-- regression in either side surfaces here, not as an "ambiguous notes" error
-- at runtime.
folderNoteCoexistenceSpec :: Spec
folderNoteCoexistenceSpec =
  describe "folder note coexistence with nested index folders (#542)" $ do
    it "foo/index.md and foo/index/index.md decode to distinct LML routes" . hedgehog $ do
      let folderNote = mkLmlRouteFromFilePath @_ @SomeExt "foo/index.md"
          nestedIndex = mkLmlRouteFromFilePath @_ @SomeExt "foo/index/index.md"
      folderNote === Just r1 -- R "foo"
      nestedIndex === Just r1Index -- R "foo" :| ["index"]
      assert (folderNote /= nestedIndex)
    it "foo/index/bar.md decodes to a third distinct LML route" . hedgehog $ do
      mkLmlRouteFromFilePath @_ @SomeExt "foo/index/bar.md"
        === Just (R $ "foo" :| ["index", "bar"])
    it "expandIndexSlug leaves the folder-note route alone" . hedgehog $ do
      -- foo/index.md → R("foo"). Length 1, no expansion. Its URL stays /foo/
      -- (or /foo.html in direct mode). The fix must not touch this case —
      -- breaking it would re-collide foo.md and foo/index.md at the
      -- IxNote level.
      expandIndexSlug (unRoute r1) === unRoute r1
    it "expandIndexSlug only re-extends the deeply-nested form" . hedgehog $ do
      -- foo/index/index.md → R("foo","index"). Length 2 ending in "index" →
      -- expansion fires. URL becomes /foo/index/, distinct from /foo/.
      expandIndexSlug (unRoute r1Index) === ("foo" :| ["index", "index"])
    it "expandIndexSlug leaves foo/index/bar.md alone" . hedgehog $ do
      -- Last slug "bar" is not "index", so no expansion regardless of depth.
      expandIndexSlug ("foo" :| ["index", "bar"]) === ("foo" :| ["index", "bar"])
    it "all three coexisting routes produce distinct HTML route slugs" . hedgehog $ do
      -- This is the load-bearing assertion: the fix's whole purpose is to
      -- separate these three notes into three different IxNote keys so they
      -- can coexist in one notebook without collision.
      let folderNoteHtml = expandIndexSlug ("foo" :| []) -- foo/index.md
          nestedIndexHtml = expandIndexSlug ("foo" :| ["index"]) -- foo/index/index.md
          siblingHtml = expandIndexSlug ("foo" :| ["index", "bar"]) -- foo/index/bar.md
      assert (folderNoteHtml /= nestedIndexHtml)
      assert (folderNoteHtml /= siblingHtml)
      assert (nestedIndexHtml /= siblingHtml)

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
