module Emanote.Pandoc.Markdown.Parser (
  parseMarkdown,
) where

import Commonmark qualified as CM
import Commonmark.Extensions qualified as CE
import Commonmark.Extensions.WikiLink qualified as WL
import Commonmark.Simple (parseMarkdownWithFrontMatter)
import Data.Aeson qualified as Aeson
import Emanote.Pandoc.Markdown.Syntax.HashTag qualified as IT
import Emanote.Pandoc.Markdown.Syntax.Highlight qualified as IH
import Relude
import Relude.Extra.Bifunctor (secondF)
import Text.Pandoc.Definition qualified as B
import Text.Pandoc.Walk qualified as W

parseMarkdown :: FilePath -> Text -> Either Text (Maybe Aeson.Value, B.Pandoc)
parseMarkdown fp =
  secondF removeNestedLinks
    . parseMarkdownWithFrontMatter @Aeson.Value parserSpec fp
  where
    parserSpec =
      -- As the commonmark documentation states, pipeTableSpec should be placed after
      -- fancyListSpec and defaultSyntaxSpec to avoid bad results when parsing
      -- non-table lines.
      -- see https://github.com/jgm/commonmark-hs/issues/52
      baseExtsSansPipeTable
        <> gfmExtensionsSansPipeTable
        <> CE.pipeTableSpec
        <> WL.wikilinkSpec
        -- ASK: Can we conditionally disable this?
        -- cf. https://github.com/srid/emanote/issues/167
        <> IT.hashTagSpec
        <> IH.highlightSpec
    baseExtsSansPipeTable =
      mconcat
        [ CE.fancyListSpec
        , CE.footnoteSpec
        , CE.mathSpec
        , CE.smartPunctuationSpec
        , CE.definitionListSpec
        , CE.attributesSpec
        , CE.rawAttributeSpec
        , CE.fencedDivSpec
        , CE.bracketedSpanSpec
        , CE.autolinkSpec
        , CM.defaultSyntaxSpec
        ]
    gfmExtensionsSansPipeTable =
      CE.emojiSpec
        <> CE.strikethroughSpec
        <> CE.autolinkSpec
        <> CE.autoIdentifiersSpec
        <> CE.taskListSpec

-- commonmark-hs can emit illegal nested links when the autolink extension sees
-- a URI inside explicit link text. Keep the outer link and collapse inner links
-- back to their displayed text.
removeNestedLinks :: B.Pandoc -> B.Pandoc
removeNestedLinks =
  W.walk $ \case
    B.Link attr inlines target ->
      B.Link attr (plainifyLinks inlines) target
    x ->
      x
  where
    plainifyLinks :: [B.Inline] -> [B.Inline]
    plainifyLinks =
      W.walk $ \case
        B.Link _ inlines _ ->
          B.Str $ WL.plainify inlines
        x ->
          x
