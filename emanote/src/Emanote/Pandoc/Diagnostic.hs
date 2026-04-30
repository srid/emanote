{- | User-visible diagnostic surfaces emitted by Emanote into a note's
Pandoc AST.

Emanote tells the reader something went wrong by injecting a styled block or
inline span into the rendered note. Three trigger sites exist today:

* YAML cascade parse failures — surfaced as a banner on the affected notes
  (see 'Emanote.Model.Note.errorDiv', issue #285).
* Broken or ambiguous wiki/Markdown links — surfaced as a strikethrough plus
  an aside (see 'Emanote.Pandoc.Renderer.Url.renderSomeInlineRefWith').
* Cyclic note embeds — surfaced as a placeholder block (see
  'Emanote.Pandoc.Renderer.Embed.renderCyclicEmbedSplice', issue #362).

This module owns the *AST shape* of those diagnostics: every error block /
span carries the universal @emanote:error@ marker class plus an
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
  errorInlineAside,
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

{- | Wrap inlines in an Emanote-emitted aside 'B.Span'. Used by inline
diagnostics (broken / ambiguous links) where the marker sits next to the
offending fragment in surrounding prose.
-}
errorInlineAside :: Text -> [B.Inline] -> B.Inline
errorInlineAside category =
  B.Span ("", errorClasses category, [])

errorClasses :: Text -> [Text]
errorClasses category
  | T.null category = ["emanote:error"]
  | otherwise = ["emanote:error", "emanote:error:" <> category]
