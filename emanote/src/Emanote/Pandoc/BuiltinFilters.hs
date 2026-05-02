module Emanote.Pandoc.BuiltinFilters (
  preparePandoc,
) where

import Emanote.Pandoc.ExternalLink (setExternalLinkIcon)
import Emanote.Pandoc.Markdown.Syntax.HashTag qualified as HT
import Emanote.Route.SiteRoute.Type (encodeTagIndexUrl)
import Relude
import Text.Pandoc.Definition qualified as B
import Text.Pandoc.Walk qualified as W

preparePandoc :: (W.Walkable B.Inline b) => b -> b
preparePandoc =
  linkifyInlineTags
    >>> fixEmojiFontFamily
    >>> setExternalLinkIcon
    >>> flattenNestedLinks

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
      toText . encodeTagIndexUrl . toList . HT.deconstructTag

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

-- Unwrap any @Link@ nested inside another @Link@'s label, keeping the inner
-- inlines and dropping the inner link's target. The @autolink@ extension in
-- @commonmark-hs@ greedily wraps bare URLs in their own @Link@ even inside a
-- parent link's label — including positions wrapped in @Strong@/@Emph@/@Span@
-- — and Pandoc renderers split the result into multiple @\<a>@ tags.
-- See https://github.com/srid/emanote/issues/349.
flattenNestedLinks :: (W.Walkable B.Inline b) => b -> b
flattenNestedLinks = W.walk $ \case
  B.Link attr inlines target -> B.Link attr (W.walk dropLinks inlines) target
  x -> x
  where
    dropLinks :: [B.Inline] -> [B.Inline]
    dropLinks = concatMap $ \case
      B.Link _ inner _ -> inner
      x -> [x]
