{- | Site-authored interactive-JS bundling surface.

Owns how the @\<script type=\"module\"\>@ entry and its companion
@\<script type=\"importmap\"\>@ get rendered into the page \<head\>.
Adding a new behavior is a one-line addition to 'emanoteJsModuleNames'
HERE, plus a matching @import '@@emanote\/\<name\>'@ line in
@_emanote-static\/js\/main.js@ — the two lists must agree (see
issue #669 for consolidation work).

== What's volatile vs. what's scaffolding

Only the cache-busting protocol — 'jsUrl' / 'importmapUrl', currently
@?t=\<mtime\>@ via 'SR.siteRouteUrl' — is expected to evolve. A future
move to content-hashed URLs (see @ema/issues/20@) or service-worker
invalidation lives there. Everything else (the manifest, the importmap
JSON shape, the @\<script\>@ tags) is W3C-spec-stable scaffolding,
co-located here for locality, not for shared change rate.

== Why importmap rather than direct imports

'SR.siteRouteUrl' appends @?t=\<mtime\>@ to static-file URLs in live
mode, but ES module imports of @.\/morph.js@ resolve to a queryless
URL per the HTML spec — so the @?t=@ on @main.js@ wouldn't propagate
to its transitive imports. The importmap turns each module's bare
specifier into a versioned URL, so editing any single module
invalidates exactly that file's cache key. See issue #643 for the
design rationale and trade-offs.
-}
module Emanote.View.JsBundle (
  emanoteJsBundle,
) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Text qualified as T
import Emanote.Model.Type (Model)
import Emanote.Model.Type qualified as M
import Emanote.Route.SiteRoute.Class qualified as SR
import Relude
import System.FilePath ((</>))
import Text.Blaze.Html ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

{- | Bare names (without @.js@ extension) of every behavior module under
@_emanote-static\/js\/@ that's loaded via the importmap. The cabal
@data-files@ glob is the source-of-truth deployment manifest, but the
importmap has to enumerate names so we can compose bare specifiers.
'main' is loaded as the entry script, not via the importmap, so it
doesn't appear here.
-}
emanoteJsModuleNames :: [Text]
emanoteJsModuleNames =
  ["morph", "theme-toggle", "code-copy", "toc-spy", "footnote-popup"]

{- | Render the importmap + module entry script tags. Drop into the page
\<head\> via a Heist splice; safe to call multiple times per render
(each call is pure, only reads from the model).

The importmap script is tagged @im-preserve@ (idiomorph keeps it
across morph) and @data-ema-skip@ (Ema's @reloadScripts@ skips it).
Both are necessary in live-server mode: a morphed-in second importmap
hits the HTML spec's \"already-resolved specifier\" rule, so the
browser silently drops every bare-name remapping and emits a console
warning per entry. Worse, the resulting empty importmap leaves any
re-evaluated module's bare imports unresolvable. Preserving the first
importmap means edits to dependent modules require a hard reload
during dev (the morph path can't pick up new @?t=@ URLs through bare
specifiers) — that's the trade-off; cleaner than visible breakage.

The main module entry is left unmarked: 'reloadScripts' re-creates
it on each morph, and a changed @?t=@ on @main.js@ triggers a fresh
fetch (its top-level imports still resolve via the preserved
importmap, so they hit the cached versions of the dependencies — the
known limitation above).
-}
emanoteJsBundle :: Model -> H.Html
emanoteJsBundle model = do
  H.script
    ! A.type_ "importmap"
    ! H.customAttribute "im-preserve" "true"
    ! H.dataAttribute "ema-skip" "true"
    $ H.toHtml (importMap model)
  H.script
    ! A.type_ "module"
    ! A.src (H.toValue (jsUrl model "main"))
    $ mempty

importMap :: Model -> Text
importMap model =
  decodeUtf8
    . Aeson.encode
    $ Aeson.object
      [ "imports"
          Aeson..= Aeson.object
            [ AesonKey.fromText ("@emanote/" <> name) Aeson..= importmapUrl model name
            | name <- emanoteJsModuleNames
            ]
      ]

{- | Importmap URL targets must start with @\/@, @.\/@, or @..\/@ per the
HTML spec — bare relative paths (which 'SR.siteRouteUrl' emits, since
they resolve through @\<base\>@) get silently ignored by the
importmap parser, leaving the bare specifier unresolvable. Prefix
with @.\/@ when needed; the @\<base\>@ resolution still happens
afterwards.
-}
importmapUrl :: Model -> Text -> Text
importmapUrl model name =
  let u = jsUrl model name
   in if "/"
        `T.isPrefixOf` u
        || "./"
        `T.isPrefixOf` u
        || "../"
        `T.isPrefixOf` u
        then u
        else "./" <> u

jsUrl :: Model -> Text -> Text
jsUrl model name =
  SR.siteRouteUrl model
    $ SR.staticFileSiteRoute
    $ fromMaybe (error $ "no _emanote-static/js/" <> name <> ".js?")
    $ M.modelLookupStaticFile ("_emanote-static" </> "js" </> toString name <> ".js") model
