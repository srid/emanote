module Emanote.Pandoc.Markdown.Parser
  ( parseMarkdown,
  )
where

import Commonmark qualified as CM
import Commonmark.Extensions qualified as CE
import Commonmark.Simple (parseMarkdownWithFrontMatter)
import Data.Aeson qualified as Aeson
import Emanote.Pandoc.Markdown.Syntax.HashTag qualified as IT
import Emanote.Pandoc.Markdown.Syntax.Highlight qualified as IH
import Emanote.Pandoc.Markdown.Syntax.WikiLink qualified as WL
import Relude
import Text.Pandoc.Definition (Pandoc)

parseMarkdown :: FilePath -> Text -> Either Text (Maybe Aeson.Value, Pandoc)
parseMarkdown =
  parseMarkdownWithFrontMatter @Aeson.Value $
    -- As the commonmark documentation states, pipeTableSpec should be placed after
    -- fancyListSpec and defaultSyntaxSpec to avoid bad results when parsing
    -- non-table lines.
    -- see https://github.com/jgm/commonmark-hs/issues/52
    baseExtsSansPipeTable
      <> gfmExtensionsSansPipeTable
      <> CE.pipeTableSpec
      <> WL.wikilinkSpec
      <> IT.hashTagSpec
      <> IH.highlightSpec
  where
    baseExtsSansPipeTable =
      mconcat
        [ CE.fancyListSpec,
          CE.footnoteSpec,
          CE.mathSpec,
          CE.smartPunctuationSpec,
          CE.definitionListSpec,
          CE.attributesSpec,
          CE.rawAttributeSpec,
          CE.fencedDivSpec,
          CE.bracketedSpanSpec,
          CE.autolinkSpec,
          CM.defaultSyntaxSpec
        ]
    gfmExtensionsSansPipeTable =
      CE.emojiSpec <> CE.strikethroughSpec <> CE.autolinkSpec
        <> CE.autoIdentifiersSpec
        <> CE.taskListSpec
