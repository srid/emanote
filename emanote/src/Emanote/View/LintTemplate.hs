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

import Data.Set qualified as Set
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
a deduplicated, sorted list. An unparseable document yields no warnings —
broken HTML is a separate concern.
-}
scanRenderedHtml :: FilePath -> ByteString -> [UnboundSplice]
scanRenderedHtml fp bs = case X.parseHTML fp bs of
  Right (X.HtmlDocument _ _ nodes) -> Set.toAscList (foldMap nodeSplices nodes)
  _ -> []

nodeSplices :: X.Node -> Set UnboundSplice
nodeSplices = \case
  X.Element name attrs children ->
    elementSplice name
      <> foldMap attrSplices attrs
      <> foldMap nodeSplices children
  _ -> mempty

elementSplice :: Text -> Set UnboundSplice
elementSplice name
  | T.any (== ':') name = one (SpliceElement name)
  | otherwise = mempty

attrSplices :: (Text, Text) -> Set UnboundSplice
attrSplices (_, value) = Set.fromList (SpliceAttribute <$> attrSpliceRefs value)

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
