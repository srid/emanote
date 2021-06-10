{-# LANGUAGE TypeApplications #-}

module Emanote.Pandoc.Markdown.Parser
  ( parseMarkdown,
  )
where

import qualified Commonmark as CM
import qualified Commonmark.Extensions as CE
import qualified Data.Aeson as Aeson
import Ema.Helper.Markdown (parseMarkdownWithFrontMatter)
import qualified Emanote.Pandoc.Markdown.Syntax.HashTag as IT
import qualified Emanote.Pandoc.Markdown.Syntax.WikiLink as WL
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
