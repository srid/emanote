module Emanote.Pandoc.Markdown.Syntax.HashTag (
  hashTagSpec,
  inlineTagsInPandoc,
  getTagFromInline,
  TT.Tag (..),
  TT.TagPattern (..),
  TT.TagNode (..),
  TT.mkTagPattern,
  TT.tagMatch,
  TT.constructTag,
  TT.deconstructTag,
  TT.tagTree,
) where

import Commonmark (TokType (..))
import Commonmark qualified as CM
import Commonmark.Inlines qualified as CM
import Commonmark.Pandoc qualified as CP
import Commonmark.TokParsers (noneOfToks, symbol)
import Data.Map.Strict qualified as Map
import Data.TagTree qualified as TT
import Data.Text qualified as T
import Relude
import Text.Pandoc.Builder qualified as B
import Text.Pandoc.Walk qualified as W
import Text.Parsec qualified as P

inlineTagsInPandoc :: B.Pandoc -> [TT.Tag]
inlineTagsInPandoc = W.query $ maybeToList . getTagFromInline

getTagFromInline :: B.Inline -> Maybe TT.Tag
getTagFromInline = \case
  B.Span (_, _, Map.fromList -> attrs) _ -> do
    tag <- Map.lookup tagDataAttr attrs
    pure $ TT.Tag tag
  _ -> Nothing

class HasHashTag il where
  hashTag :: TT.Tag -> il

instance HasHashTag (CP.Cm b B.Inlines) where
  hashTag (TT.Tag tag) =
    let attrs =
          [ ("title", "Tag")
          , (tagDataAttr, tag)
          ]
        classes =
          [ "emanote:inline-tag"
          , -- This must be placed *after* the class above, to allow the user to
            -- override generic styles (of the class above)
            "emanote:inline-tag:" <> tag
          ]
     in CP.Cm $ B.spanWith ("", classes, attrs) $ B.str $ "#" <> tag

tagDataAttr :: Text
tagDataAttr = "data-tag"

hashTagSpec ::
  (Monad m, CM.IsBlock il bl, CM.IsInline il, HasHashTag il) =>
  CM.SyntaxSpec m il bl
hashTagSpec =
  mempty
    { CM.syntaxInlineParsers = [pTag]
    }
  where
    pTag ::
      (Monad m, CM.IsInline il, HasHashTag il) =>
      CM.InlineParser m il
    pTag = P.try $ do
      _ <- symbol '#'
      tag <- CM.untokenize <$> tagP
      pure $ hashTag $ TT.Tag tag
    tagP :: (Monad m) => P.ParsecT [CM.Tok] s m [CM.Tok]
    tagP = do
      s <- some (noneOfToks disallowed)
      -- A tag cannot end with a slash (which is a separator in hierarchical tags)
      guard $ not $ "/" `T.isSuffixOf` CM.untokenize s
      pure s
      where
        disallowed = [Spaces, UnicodeSpace, LineEnd] <> fmap Symbol punctuation
        punctuation = "[];:,.?!"
