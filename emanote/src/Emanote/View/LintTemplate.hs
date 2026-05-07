{- | Detect Heist template splices that have no binding. Pure: this module
returns the warnings; deciding what to /do/ with them (log, dedupe across
re-renders, surface in a UI banner) is the caller's job.

Emanote uses interpreted Heist, where 'Heist.Interpreted.runNode' leaves an
element verbatim when its name has no binding, and 'getAttributeSplice'
falls back to the literal @${name}@ text when @name@ has no binding. Both
paths fail silently in the rendered HTML. We re-parse the rendered output
and report either signal as an 'UnboundSplice'. Closes
<https://github.com/srid/emanote/issues/81>.
-}
module Emanote.View.LintTemplate (
  UnboundSplice (..),
  scanRenderedHtml,
  formatWarning,
) where

import Data.Text qualified as T
import Relude
import Text.XmlHtml qualified as X

-- | An unbound splice reference detected in rendered HTML output.
data UnboundSplice
  = SpliceElement Text
  | SpliceAttribute Text
  deriving stock (Eq, Ord, Show)

-- | Render a warning as user-facing text.
formatWarning :: UnboundSplice -> Text
formatWarning = \case
  SpliceElement nm -> "<" <> nm <> "/>"
  SpliceAttribute nm -> "${" <> nm <> "}"

{- | Re-parse rendered HTML and collect every unbound splice reference. Returns
@Left@ on a parse failure so the caller can surface that as its own
diagnostic — silence on unparseable HTML would give a false-clean lint. An
XML document is treated as having no warnings (Emanote does not emit XML
through the Heist pipeline this lints).
-}
scanRenderedHtml :: FilePath -> ByteString -> Either Text [UnboundSplice]
scanRenderedHtml fp bs = case X.parseHTML fp bs of
  Left err -> Left (toText err)
  Right (X.HtmlDocument _ _ nodes) -> Right (sortNub (foldMap nodeSplices nodes))
  Right X.XmlDocument {} -> Right []

{- | Walk an 'X.Node' (and its descendants) into a list of splice references.
Duplicates are collapsed at the document root in 'scanRenderedHtml' rather
than per node — it costs less to fold once than to merge intermediate Sets.

The colon heuristic: any element name with a @:@ is a Heist splice. Plain
HTML5 tag names never contain a colon, and SVG/MathML inlined into HTML5
uses unprefixed forms (@<svg>@, @<math>@, @<mfrac>@). If a future
legitimate use of a colon-bearing tag arises, an allow-list belongs here.
-}
nodeSplices :: X.Node -> [UnboundSplice]
nodeSplices = \case
  X.Element name attrs children ->
    [SpliceElement name | T.any (== ':') name]
      <> concatMap attrSplices attrs
      <> foldMap nodeSplices children
  _ -> []

attrSplices :: (Text, Text) -> [UnboundSplice]
attrSplices (_, value) = SpliceAttribute <$> attrSpliceRefs value

{- | Extract the names from any @${name}@ tokens in a string. A bare @${@
with no closing brace (or one that wraps a nested @${@, e.g.
@${incomplete ${valid}@) is treated as literal text — we step past it and
keep scanning, so a real splice that follows is still reported on its own.
-}
attrSpliceRefs :: Text -> [Text]
attrSpliceRefs t = case T.breakOn "${" t of
  (_, "") -> []
  (_, rest) ->
    let body = T.drop 2 rest
     in case T.breakOn "}" body of
          (name, suffix)
            | "}" `T.isPrefixOf` suffix
            , not (T.any (== '$') name) ->
                name : attrSpliceRefs (T.drop 1 suffix)
          _ -> attrSpliceRefs body
