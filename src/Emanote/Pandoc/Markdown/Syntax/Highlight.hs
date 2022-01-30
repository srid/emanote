module Emanote.Pandoc.Markdown.Syntax.Highlight
  ( highlightSpec,
  )
where

import Commonmark qualified as CM
import Commonmark.Inlines qualified as CM
import Commonmark.Pandoc qualified as CP
import Relude
import Text.Pandoc.Builder qualified as B

highlightSpec ::
  (Monad m, CM.IsInline il, HasHighlight il) =>
  CM.SyntaxSpec m il bl
highlightSpec =
  mempty
    { CM.syntaxFormattingSpecs =
        [ CM.FormattingSpec '=' True True Nothing (Just highlight) '='
        ]
    }

class HasHighlight a where
  highlight :: a -> a

instance HasHighlight (CP.Cm a B.Inlines) where
  highlight il = B.spanWith attr <$> il
    where
      attr = ("", one "highlight-inline", [])
