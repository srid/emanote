{-# LANGUAGE UndecidableInstances #-}

module Emanote.Pandoc.Markdown.Syntax.InlineTag (inlineTagSpec) where

import Commonmark (TokType (..))
import qualified Commonmark as CM
import qualified Commonmark.Inlines as CM
import qualified Commonmark.Pandoc as CP
import Commonmark.TokParsers (noneOfToks, symbol)
import qualified Text.Pandoc.Builder as B
import qualified Text.Parsec as P

newtype InlineTag = InlineTag {unInlineTag :: Text}
  deriving (Eq, Show, Ord)

class HasInlineTag il where
  inlineTag :: InlineTag -> il

instance HasInlineTag (CP.Cm b B.Inlines) where
  inlineTag (InlineTag tag) =
    CP.Cm $ B.spanWith ("", one cls, one ("title", "Tag")) $ B.str $ "#" <> tag
    where
      cls = "emanote:inline-tag"

inlineTagSpec ::
  (Monad m, CM.IsBlock il bl, CM.IsInline il, HasInlineTag il) =>
  CM.SyntaxSpec m il bl
inlineTagSpec =
  mempty
    { CM.syntaxInlineParsers = [pInlineTag]
    }
  where
    pInlineTag ::
      (Monad m, CM.IsInline il, HasInlineTag il) =>
      CM.InlineParser m il
    pInlineTag = P.try $ do
      _ <- symbol '#'
      tag <- CM.untokenize <$> inlineTagP
      pure $ inlineTag $ InlineTag tag
    inlineTagP :: Monad m => P.ParsecT [CM.Tok] s m [CM.Tok]
    inlineTagP =
      some (noneOfToks $ [Spaces, UnicodeSpace, LineEnd] <> fmap Symbol punctuation)
      where
        punctuation = "[];:,.?!"
