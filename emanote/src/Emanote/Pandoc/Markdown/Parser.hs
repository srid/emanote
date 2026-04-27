module Emanote.Pandoc.Markdown.Parser (
  parseMarkdown,
) where

import Commonmark qualified as CM
import Commonmark.Extensions qualified as CE
import Commonmark.Extensions.WikiLink qualified as WL
import Commonmark.Simple (parseMarkdownWithFrontMatter)
import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Emanote.Pandoc.Markdown.Syntax.HashTag qualified as IT
import Emanote.Pandoc.Markdown.Syntax.Highlight qualified as IH
import Relude
import Text.Pandoc.Definition (Pandoc)

parseMarkdown :: FilePath -> Text -> Either Text (Maybe Aeson.Value, Pandoc)
parseMarkdown fp md =
  parseMarkdownWithFrontMatter @Aeson.Value (syntaxSpecFor md) fp md
  where
    syntaxSpecFor md' =
      -- As the commonmark documentation states, pipeTableSpec should be placed after
      -- fancyListSpec and defaultSyntaxSpec to avoid bad results when parsing
      -- non-table lines.
      -- see https://github.com/jgm/commonmark-hs/issues/52
      baseExtsSansPipeTable md'
        <> gfmExtensionsSansPipeTable md'
        <> whenPresent "|" CE.pipeTableSpec
        <> whenPresent "[[" WL.wikilinkSpec
        <> whenPresent "#" IT.hashTagSpec
        <> whenPresent "==" IH.highlightSpec
      where
        whenPresent needle spec =
          bool mempty spec $ needle `T.isInfixOf` md'

    baseExtsSansPipeTable md' =
      let whenPresent needle spec =
            bool mempty spec $ needle `T.isInfixOf` md'
       in mconcat
            [ CE.fancyListSpec
            , whenPresent "[^" CE.footnoteSpec
            , whenPresent "$" CE.mathSpec
            , CE.smartPunctuationSpec
            , whenPresent "\n:" CE.definitionListSpec
            , whenPresent "{" CE.attributesSpec
            , whenPresent "{" CE.rawAttributeSpec
            , whenPresent ":::" CE.fencedDivSpec
            , whenPresent "[" CE.bracketedSpanSpec
            , autolinkSpecFor md'
            , CM.defaultSyntaxSpec
            ]

    gfmExtensionsSansPipeTable md' =
      let whenPresent needle spec =
            bool mempty spec $ needle `T.isInfixOf` md'
       in mconcat
            [ whenPresent ":" CE.emojiSpec
            , whenPresent "~~" CE.strikethroughSpec
            , CE.autoIdentifiersSpec
            , bool mempty CE.taskListSpec $ any (`T.isInfixOf` md') ["[ ]", "[x]", "[X]"]
            ]

    autolinkSpecFor md' =
      bool mempty CE.autolinkSpec
        $ any (`T.isInfixOf` md') ["http://", "https://", "www.", "@", "<"]
