{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Emanote.Pandoc.Markdown.Syntax.Highlight
  ( highlightSpec,
  )
where

import qualified Commonmark as CM
import qualified Commonmark.Html as CH
import qualified Commonmark.Inlines as CM

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

instance HasHighlight (CH.Html a) where
  highlight x = CH.htmlInline "mark" (Just x)

instance
  (HasHighlight i, Monoid i) =>
  HasHighlight (CM.WithSourceMap i)
  where
  highlight x = (highlight <$> x) <* CM.addName "highlight"
