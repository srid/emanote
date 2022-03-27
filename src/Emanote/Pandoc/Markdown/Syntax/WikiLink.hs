{-# LANGUAGE DeriveAnyClass #-}

module Emanote.Pandoc.Markdown.Syntax.WikiLink (
  WikiLink,
  WikiLinkType (..),
  wikilinkSpec,
  mkWikiLinkFromRoute,
  delineateLink,
  wikilinkInline,
  wikiLinkInlineRendered,
  mkWikiLinkFromInline,
  allowedWikiLinks,

  -- * Anchors in URLs
  Anchor,
  anchorSuffix,

  -- * Pandoc helper, which use wikilink somehow
  plainify,
) where

import Commonmark qualified as CM
import Commonmark.Pandoc qualified as CP
import Commonmark.TokParsers qualified as CT
import Control.Monad (liftM2)
import Data.Aeson (ToJSON (toJSON))
import Data.Data (Data)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Emanote.Route.Ext qualified as Ext
import Emanote.Route.R (R (..))
import Network.URI.Encode qualified as UE
import Network.URI.Slug (Slug)
import Network.URI.Slug qualified as Slug
import Relude
import Text.Megaparsec qualified as M
import Text.Pandoc.Builder qualified as B
import Text.Pandoc.Walk qualified as W
import Text.Parsec qualified as P
import Text.Read (Read (readsPrec))
import Text.Show qualified (Show (show))

{- | Represents the "Foo" in [[Foo]]

 As wiki links may contain multiple path components, it can also represent
 [[Foo/Bar]], hence we use nonempty slug list.
-}
newtype WikiLink = WikiLink {unWikiLink :: NonEmpty Slug}
  deriving stock (Eq, Ord, Typeable, Data)

instance ToJSON WikiLink where
  toJSON = toJSON . wikilinkUrl

instance Show WikiLink where
  show wl =
    toString $ "[[" <> wikilinkUrl wl <> "]]"

-- -----------------
-- Making wiki links
-- -----------------

mkWikiLinkFromRoute :: R ext -> WikiLink
mkWikiLinkFromRoute (R slugs) = WikiLink slugs

mkWikiLinkFromUrl :: (Monad m, Alternative m) => Text -> m WikiLink
mkWikiLinkFromUrl s = do
  slugs <- maybe empty pure $ nonEmpty $ Slug.decodeSlug <$> T.splitOn "/" s
  pure $ WikiLink slugs

mkWikiLinkFromInline :: B.Inline -> Maybe (WikiLink, [B.Inline])
mkWikiLinkFromInline inl = do
  B.Link (_id, _class, otherAttrs) is (url, tit) <- pure inl
  (Left (_, wl), _manchor) <- delineateLink (otherAttrs <> one ("title", tit)) url
  pure (wl, is)

-- | An URL anchor without the '#'
newtype Anchor = Anchor Text
  deriving newtype (Eq, Show, Ord)

mkAnchor :: String -> Maybe Anchor
mkAnchor ('#' : name) = pure $ Anchor $ toText name
mkAnchor _ = Nothing

anchorSuffix :: Maybe Anchor -> Text
anchorSuffix = maybe "" (\(Anchor a) -> "#" <> a)

dropUrlAnchor :: Text -> (Text, Maybe Anchor)
dropUrlAnchor = second (mkAnchor . toString) . T.breakOn "#"

{- | Given a Pandoc Link node, apparaise what kind of link it is.

 * Nothing, if the link is an absolute URL
 * Just (Left wl), if a wiki-link
 * Just (Right fp), if a relative path (not a wiki-link)
-}
delineateLink :: [(Text, Text)] -> Text -> Maybe (Either (WikiLinkType, WikiLink) FilePath, Maybe Anchor)
delineateLink (Map.fromList -> attrs) url = do
  -- Must be relative
  guard $ not $ "://" `T.isInfixOf` url
  wikiLink <|> internalLink
  where
    wikiLink = do
      wlType :: WikiLinkType <- readMaybe . toString <=< Map.lookup htmlAttr $ attrs
      -- Ignore anchors until https://github.com/srid/emanote/discussions/105
      let (s, manc) = dropUrlAnchor url
      wl <- mkWikiLinkFromUrl s
      pure (Left (wlType, wl), manc)
    internalLink = do
      -- Avoid links like "mailto:", "magnet:", etc.
      -- An easy way to parse them is to look for colon character.
      --
      -- This does mean that "Foo: Bar.md" cannot be linked to this way, however
      -- the user can do it using wiki-links.
      guard $ not $ ":" `T.isInfixOf` url
      let (s, manc) = dropUrlAnchor url
      guard $ not $ T.null s -- Same page anchors
      pure (Right $ UE.decode (toString s), manc)

-- ---------------------
-- Converting wiki links
-- ---------------------

-- | [[Foo/Bar]] -> "Foo/Bar"
wikilinkUrl :: WikiLink -> Text
wikilinkUrl =
  T.intercalate "/" . fmap Slug.unSlug . toList . unWikiLink

wikilinkInline :: WikiLinkType -> WikiLink -> B.Inlines -> B.Inlines
wikilinkInline typ wl = B.linkWith attrs (wikilinkUrl wl) ""
  where
    attrs = ("", [], [(htmlAttr, show typ)])

wikiLinkInlineRendered :: B.Inline -> Maybe Text
wikiLinkInlineRendered x = do
  (wl, inl) <- mkWikiLinkFromInline x
  pure $ case nonEmpty inl of
    Nothing -> show wl
    Just _ ->
      let inlStr = plainify inl
       in if inlStr == wikilinkUrl wl
            then show wl
            else "[[" <> wikilinkUrl wl <> "|" <> plainify inl <> "]]"

{- | Return the various ways to link to a route (ignoring ext)

 Foo/Bar/Qux.md -> [[Qux]], [[Bar/Qux]], [[Foo/Bar/Qux]]

 All possible combinations of Wikilink type use is automatically included.
-}
allowedWikiLinks :: HasCallStack => R @Ext.SourceExt ext -> NonEmpty (WikiLinkType, WikiLink)
allowedWikiLinks r =
  let wls = fmap WikiLink $ tailsNE $ unRoute r
      typs :: NonEmpty WikiLinkType = NE.fromList [minBound .. maxBound]
   in liftM2 (,) typs wls
  where
    tailsNE =
      NE.fromList . mapMaybe nonEmpty . tails . toList

-------------------------
-- Parser
--------------------------

{- | A # prefix or suffix allows semantically distinct wikilinks

 Typically called branching link or a tag link, when used with #.
-}
data WikiLinkType
  = -- | [[Foo]]
    WikiLinkNormal
  | -- | [[Foo]]#
    WikiLinkBranch
  | -- | #[[Foo]]
    WikiLinkTag
  | -- | ![[Foo]]
    WikiLinkEmbed
  deriving stock (Eq, Show, Ord, Typeable, Data, Enum, Bounded, Generic)
  deriving anyclass (ToJSON)

instance Read WikiLinkType where
  readsPrec _ s
    | s == show WikiLinkNormal = [(WikiLinkNormal, "")]
    | s == show WikiLinkBranch = [(WikiLinkBranch, "")]
    | s == show WikiLinkTag = [(WikiLinkTag, "")]
    | s == show WikiLinkEmbed = [(WikiLinkEmbed, "")]
    | otherwise = []

-- | The HTML 'data attribute' storing the wiki-link type.
htmlAttr :: Text
htmlAttr = "data-wikilink-type"

class HasWikiLink il where
  wikilink :: WikiLinkType -> WikiLink -> Maybe il -> il

instance HasWikiLink (CP.Cm b B.Inlines) where
  wikilink typ wl il =
    CP.Cm $ wikilinkInline typ wl $ maybe mempty CP.unCm il

{- | Like `Commonmark.Extensions.Wikilinks.wikilinkSpec` but Zettelkasten-friendly.

 Compared with the official extension, this has two differences:

 - Supports flipped inner text, eg: `[[Foo | some inner text]]`
 - Supports neuron folgezettel, i.e.: #[[Foo]] or [[Foo]]#
-}
wikilinkSpec ::
  (Monad m, CM.IsInline il, HasWikiLink il) =>
  CM.SyntaxSpec m il bl
wikilinkSpec =
  mempty
    { CM.syntaxInlineParsers =
        [ P.try $
            P.choice
              [ P.try (CT.symbol '#' *> pWikilink WikiLinkTag)
              , P.try (CT.symbol '!' *> pWikilink WikiLinkEmbed)
              , P.try (pWikilink WikiLinkBranch <* CT.symbol '#')
              , P.try (pWikilink WikiLinkNormal)
              ]
        ]
    }
  where
    pWikilink typ = do
      replicateM_ 2 $ CT.symbol '['
      P.notFollowedBy (CT.symbol '[')
      url <-
        CM.untokenize <$> many (satisfyNoneOf [isPipe, isAnchor, isClose])
      wl <- mkWikiLinkFromUrl url
      -- We ignore the anchor until https://github.com/srid/emanote/discussions/105
      _anchor <-
        M.optional $
          CM.untokenize
            <$> ( CT.symbol '#'
                    *> many (satisfyNoneOf [isPipe, isClose])
                )
      title <-
        M.optional $
          -- TODO: Should parse as inline so link text can be formatted?
          CM.untokenize
            <$> ( CT.symbol '|'
                    *> many (satisfyNoneOf [isClose])
                )
      replicateM_ 2 $ CT.symbol ']'
      return $ wikilink typ wl (fmap CM.str title)
    satisfyNoneOf toks =
      CT.satisfyTok $ \t -> not $ or $ toks <&> \tok -> tok t
    isAnchor =
      isSymbol '#'
    isPipe =
      isSymbol '|'
    isClose =
      isSymbol ']'
    isSymbol c =
      CT.hasType (CM.Symbol c)

-- | Convert Pandoc AST inlines to raw text.
plainify :: [B.Inline] -> Text
plainify = W.query $ \case
  B.Str x -> x
  B.Code _attr x -> x
  B.Space -> " "
  B.SoftBreak -> " "
  B.LineBreak -> " "
  -- TODO: if fmt is html, we should strip the html tags
  B.RawInline _fmt s -> s
  -- Ignore "wrapper" inlines like span.
  B.Span _ _ -> ""
  -- TODO: How to wrap math stuff here?
  B.Math _mathTyp s -> s
  -- Wiki-links must be displayed using its show instance (which returns its
  -- human-readable representation)
  (mkWikiLinkFromInline -> Just wl) ->
    show wl
  -- Ignore the rest of AST nodes, as they are recursively defined in terms of
  -- `Inline` which `W.query` will traverse again.
  _ -> ""
