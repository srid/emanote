module Emanote.Pandoc.Filter.Builtin
  ( prepareNoteDoc,
    preparePandoc,
  )
where

import Emanote.Model.Type (Model)
import qualified Emanote.Pandoc.Markdown.Syntax.HashTag as HT
import qualified Emanote.Route.SiteRoute.Class as SR
import qualified Text.Pandoc.Definition as B
import qualified Text.Pandoc.Walk as W

prepareNoteDoc :: Model -> B.Pandoc -> B.Pandoc
prepareNoteDoc model =
  preparePandoc model
    >>> withoutH1 -- Because, handling note title separately

preparePandoc :: W.Walkable B.Inline b => Model -> b -> b
preparePandoc model =
  linkifyInlineTags model
    >>> fixEmojiFontFamily

-- HashTag.hs generates a Span for inline tags.
-- Here, we must link them to the special tag index page.
linkifyInlineTags :: W.Walkable B.Inline b => Model -> b -> b
linkifyInlineTags model =
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
      SR.siteRouteUrl model . SR.tagIndexRoute . toList . HT.deconstructTag

withoutH1 :: B.Pandoc -> B.Pandoc
withoutH1 (B.Pandoc meta (B.Header 1 _ _ : rest)) =
  B.Pandoc meta rest
withoutH1 doc =
  doc

-- Undo font-familly on emoji spans, so the browser uses an emoji font.
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