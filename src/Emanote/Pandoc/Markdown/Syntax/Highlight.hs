{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Emanote.Pandoc.Markdown.Syntax.Highlight
  ( highlightSpec,
  )
where

import qualified Commonmark as CM
import qualified Commonmark.Inlines as CM
import qualified Commonmark.Pandoc as CP
import Relude
import qualified Text.Pandoc.Builder as B

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
