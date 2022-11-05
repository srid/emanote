module Emanote.Pandoc.Link where

import Commonmark.Extensions.WikiLink qualified as WL
import Relude
import Text.Pandoc.Definition qualified as B

-- | A Pandoc inline node that refers to something else.
--
-- There are, currently, only two possible nodes: link & image.
data InlineRef
  = InlineLink
  | InlineImage
  deriving stock (Eq, Show)

parseInlineRef :: B.Inline -> Maybe (InlineRef, B.Attr, [B.Inline], (Text, Text))
parseInlineRef = \case
  B.Link attr is (url, tit) ->
    pure (InlineLink, attr, is, (url, tit))
  B.Image attr is (url, tit) ->
    pure (InlineImage, attr, is, (url, tit))
  _ ->
    Nothing

-- | Given an inline that is known to be an InlineRef, reconstruct and return
-- its orginal Markdown source.
unParseLink :: HasCallStack => B.Inline -> Text
unParseLink inl =
  case WL.wikiLinkInlineRendered inl of
    Just url ->
      url
    Nothing ->
      let (inlRef, _, is, (url, _tit)) = parseInlineRefMust inl
          prefix = if inlRef == InlineImage then "![" else "["
       in prefix <> WL.plainify is <> "](" <> url <> ")"
  where
    parseInlineRefMust =
      fromMaybe (error "Non-InlineRef Inline") . parseInlineRef
