module Emanote.Pandoc.BuiltinFilters
  ( prepareNoteDoc,
    preparePandoc,
    markLinksWithIcon,
  )
where

import Emanote.Model.Note qualified as N
import Emanote.Pandoc.Markdown.Syntax.HashTag qualified as HT
import Emanote.Route (encodeRoute)
import Emanote.Route.SiteRoute.Type (encodeTagIndexR)
import Optics.Core ((^.))
import Relude
import Text.Pandoc.Definition qualified as B
import Text.Pandoc.Walk qualified as W
import Text.Parsec qualified as P
import Text.Parsec.Char qualified as PC

-- TODO: Run this in `parseNote`?
prepareNoteDoc :: N.Note -> B.Pandoc
prepareNoteDoc note =
  preparePandoc $ note ^. N.noteDoc

preparePandoc :: W.Walkable B.Inline b => b -> b
preparePandoc =
  linkifyInlineTags
    >>> fixEmojiFontFamily
    >>> markLinksWithIcon

-- HashTag.hs generates a Span for inline tags.
-- Here, we must link them to the special tag index page.
linkifyInlineTags :: W.Walkable B.Inline b => b -> b
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
fixEmojiFontFamily :: W.Walkable B.Inline b => b -> b
fixEmojiFontFamily =
  W.walk $ \case
    B.Span (id', classes, attrs) is
      | classes == ["emoji"] ->
          let emojiFontAttr = ("style", "font-family: emoji")
              newAttrs = attrs <> one emojiFontAttr
           in B.Span (id', classes, newAttrs) is
    x -> x

-- Adds a data-linkicon=show attribute to external links that contain some text in their description, provided that they do not already have a data-linkicon attribute.
markLinksWithIcon :: W.Walkable B.Inline b => b -> b
markLinksWithIcon =
  W.walk $ \case
    B.Link (id', classes, attrs) inlines (url, title)
      | hasURIScheme url && containsText inlines ->
        let showLinkIconAttr = ("data-linkicon", "show")
            newAttrs = insert attrs showLinkIconAttr
         in B.Link (id', classes, newAttrs) inlines (url, title)
    x -> x
  where
    insert as a
      | fst a `elem` (fst <$> as) = as
      | otherwise = a : as
    hasURIScheme =
      isRight . P.parse schemeP ""
      where
        schemeP = do
          c <- PC.letter
          cs <- P.many (PC.alphaNum P.<|> P.oneOf ".-+")
          _ <- PC.char ':'
          return (c : cs)
    containsText =
      getAny
        . W.query
          ( \case
              B.Str _ -> Any True
              B.Code _ _ -> Any True
              B.Math _ _ -> Any True
              _ -> Any False
          )
