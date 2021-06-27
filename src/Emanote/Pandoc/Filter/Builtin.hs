module Emanote.Pandoc.Filter.Builtin where

import Emanote.Model.Type (Model)
import qualified Emanote.Pandoc.Markdown.Syntax.HashTag as HT
import qualified Emanote.Route.SiteRoute.Class as SR
import qualified Text.Pandoc.Definition as B
import qualified Text.Pandoc.Walk as W

prepareNoteDoc :: Model -> B.Pandoc -> B.Pandoc
prepareNoteDoc model =
  linkifyInlineTags
    >>> withoutH1 -- Because, handling note title separately
  where
    -- HashTag.hs generates a Span for inline tags.
    -- Here, we must link them to the special tag index page.
    linkifyInlineTags =
      W.walk $ \case
        inline@(B.Span _attr is) ->
          if
              | Just inlineTag <- HT.getTagFromInline inline ->
                B.Link mempty is (tagUrl inlineTag, "Tag")
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
