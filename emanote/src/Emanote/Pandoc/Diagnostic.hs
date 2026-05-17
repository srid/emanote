{- | User-visible diagnostic surfaces emitted by Emanote into a note's
Pandoc AST.

Emanote tells the reader something went wrong by injecting a styled block or
inline span into the rendered note. Two trigger sites use the AST helper
in this module:

* YAML cascade parse failures — surfaced as a banner on the affected notes
  (see 'Emanote.Model.Note.errorDiv', issue #285).
* Cyclic note embeds — surfaced as a placeholder block (see
  'Emanote.Pandoc.Renderer.Embed.renderCyclicEmbedSplice', issue #362).

Inline diagnostics for broken and ambiguous wikilinks live in their own
Heist templates (@templates/components/broken-link.tpl@ and
@templates/components/ambiguous-link.tpl@; see issues #221 and #712); they
emit raw HTML and don't go through this module.

This module owns the *AST shape* of the templated-block diagnostics: every
error block carries the universal @emanote:error@ marker class plus an
@emanote:error:\<category\>@ variant sub-class. Variant-specific styling
lives in the @pandoc.rewriteClass@ map in @emanote/default/index.yaml@; a
future "diagnostics mode" CSS rule can target @[class~="emanote:error"]@
without touching every trigger site.

== Icon vocabulary

Each diagnostic carries an icon prefix that distinguishes its category at a
glance. The vocabulary is intentional:

* @↺@ — cyclic embed (the loop symbol mirrors the recursion that was cut)
* @❌@ — broken link (target route doesn't resolve)
* @❗@ — ambiguous link (more than one resolvable target; user must
  disambiguate)

The YAML banner uses a bolded header rather than an icon because it lists
multiple errors at once.
-}
module Emanote.Pandoc.Diagnostic (
  errorBlock,
  errorClasses,
  errorVariantClass,
  luaFilterCategory,
) where

import Data.Text qualified as T
import Relude
import Text.Pandoc.Definition qualified as B

{- | Wrap one or more 'B.Block's in an Emanote-emitted error 'B.Div' carrying
@emanote:error@ plus an @emanote:error:\<category\>@ variant class.
-}
errorBlock :: Text -> [B.Block] -> B.Block
errorBlock category =
  B.Div ("", errorClasses category, [])

-- | The full class list for a category: the universal marker plus the variant.
errorClasses :: Text -> [Text]
errorClasses category
  | T.null category = ["emanote:error"]
  | otherwise = ["emanote:error", errorVariantClass category]

-- | The variant-only class for a category (the universal @emanote:error@ marker is /not/ included).
errorVariantClass :: Text -> Text
errorVariantClass category = "emanote:error:" <> category

{- | Category for Pandoc-Lua-filter-emitted errors — the contract shared between
the bundled @lua-filters/diagram.lua@ (and the injected @emanote.error_block@
helper) on the emit side, and 'Emanote.View.Template.extractInPlaceFilterErrors'
on the recover side.
-}
luaFilterCategory :: Text
luaFilterCategory = "lua-filter"
