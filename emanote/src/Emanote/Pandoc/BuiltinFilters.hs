module Emanote.Pandoc.BuiltinFilters (
  preparePandoc,
  flattenNestedLinks,
) where

import Emanote.Pandoc.ExternalLink (setExternalLinkIcon)
import Emanote.Pandoc.Markdown.Syntax.HashTag qualified as HT
import Emanote.Route (encodeRoute)
import Emanote.Route.SiteRoute.Type (encodeTagIndexR)
import Relude
import Text.Pandoc.Definition qualified as B
import Text.Pandoc.Walk qualified as W

preparePandoc :: (W.Walkable B.Inline b) => b -> b
preparePandoc =
  linkifyInlineTags
    >>> fixEmojiFontFamily
    >>> setExternalLinkIcon

-- HashTag.hs generates a Span for inline tags.
-- Here, we must link them to the special tag index page.
linkifyInlineTags :: (W.Walkable B.Inline b) => b -> b
linkifyInlineTags =
  W.walk $ \case
    inline@(B.Span attr is) ->
      if
        | Just inlineTag <- HT.getTagFromInline inline ->
            B.Span attr [B.Link mempty is (tagUrl inlineTag, "Tag")]
        | otherwise ->
            inline
    x ->
      x
  where
    tagUrl =
      toText . encodeRoute . encodeTagIndexR . toList . HT.deconstructTag

-- Undo font-family on emoji spans, so the browser uses an emoji font.
-- Ref: https://github.com/jgm/commonmark-hs/blob/3d545d7afa6c91820b4eebf3efeeb80bf1b27128/commonmark-extensions/src/Commonmark/Extensions/Emoji.hs#L30-L33
fixEmojiFontFamily :: (W.Walkable B.Inline b) => b -> b
fixEmojiFontFamily =
  W.walk $ \case
    B.Span (id', classes, attrs) is
      | classes == ["emoji"] ->
          let emojiFontAttr = ("style", "font-family: emoji")
              newAttrs = attrs <> one emojiFontAttr
           in B.Span (id', classes, newAttrs) is
    x -> x

{- | Replace any @Link@ nested inside a parent @Link@'s label with its inner
inlines, dropping the inner link's target.

The @autolink@ extension in @commonmark-hs@ greedily turns bare URLs and email
addresses into autolinks even when they appear inside another link's label —
including positions wrapped in emphasis (@Strong@, @Emph@, @Span@, …). After
Pandoc translation, this surfaces as @Link [… Link …] target@, which renderers
split into multiple @\<a>@ tags.

This pass runs as the postcondition of Emanote's whole pre-render pipeline
(@preparePandoc@ + user-supplied Pandoc filters) so that the AST handed to the
renderer is guaranteed not to contain nested links, regardless of what user
filters did. Non-link container inlines and @Image@ children are preserved
structurally; only @Link@ wrappers are unwrapped.

See <https://github.com/srid/emanote/issues/349>.
-}
flattenNestedLinks :: (W.Walkable B.Inline b) => b -> b
flattenNestedLinks = W.walk $ \case
  B.Link attr inlines target -> B.Link attr (W.walk dropLinks inlines) target
  x -> x
  where
    dropLinks :: [B.Inline] -> [B.Inline]
    dropLinks = concatMap $ \case
      B.Link _ inner _ -> inner
      x -> [x]
