{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Emanote.Pandoc.Markdown.Syntax.WikiLink
  ( WikiLink,
    WikiLinkType (..),
    wikilinkSpec,
    mkWikiLinkFromRoute,
    delineateLink,
    wikilinkInline,
    inlineToWikiLink,
    allowedWikiLinks,
  )
where

import qualified Commonmark as CM
import qualified Commonmark.Pandoc as CP
import qualified Commonmark.TokParsers as CT
import Control.Monad (liftM2)
import Data.Data (Data)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Ema (Slug (unSlug))
import qualified Ema
import Emanote.Route (ModelRoute, R (R, unRoute), lmlRouteCase, modelRouteCase)
import qualified Network.URI.Encode as UE
import qualified Text.Megaparsec as M
import qualified Text.Pandoc.Builder as B
import qualified Text.Parsec as P
import Text.Read (Read (readsPrec))
import qualified Text.Show (Show (show))

-- | Represents the "Foo" in [[Foo]]
--
-- As wiki links may contain multiple path components, it can also represent
-- [[Foo/Bar]], hence we use nonempty slug list.
newtype WikiLink = WikiLink {unWikiLink :: NonEmpty Slug}
  deriving (Eq, Ord, Typeable, Data)

instance Show WikiLink where
  show wl =
    toString $ "[[" <> wikilinkUrl wl <> "]]"

-- | Given a Pandoc Link node, apparaise what kind of link it is.
--
-- * Nothing, if the link is an absolute URL
-- * Just (Left wl), if a wiki-link
-- * Just (Right fp), if a relative path (not a wiki-link)
delineateLink :: [(Text, Text)] -> Text -> Maybe (Either (WikiLinkType, WikiLink) FilePath)
delineateLink (Map.fromList -> attrs) url = do
  -- Let absolute URLs pass through
  guard $ not $ "://" `T.isInfixOf` url
  -- URLs with anchors are ignored (such as in -/tags#foo).
  guard $ not $ "#" `T.isInfixOf` url
  fmap Left wikiLink <|> fmap Right hyperLinks
  where
    wikiLink = do
      wlType :: WikiLinkType <- readMaybe . toString <=< Map.lookup htmlAttr $ attrs
      wl <- mkWikiLinkFromUrl url
      pure (wlType, wl)
    hyperLinks = do
      -- Avoid links like "mailto:", "magnet:", etc.
      -- An easy way to parse them is to look for colon character.
      --
      -- This does mean that "Foo: Bar.md" cannot be linked to this way, however
      -- the user can do it using wiki-links.
      guard $ not $ ":" `T.isInfixOf` url
      pure $ UE.decode (toString url)

mkWikiLinkFromRoute :: R ext -> WikiLink
mkWikiLinkFromRoute (R slugs) = WikiLink slugs

mkWikiLinkFromUrl :: (Monad m, Alternative m) => Text -> m WikiLink
mkWikiLinkFromUrl s = do
  slugs <- maybe empty pure $ nonEmpty $ Ema.decodeSlug <$> T.splitOn "/" s
  pure $ WikiLink slugs

wikilinkUrl :: WikiLink -> Text
wikilinkUrl =
  T.intercalate "/" . fmap unSlug . toList . unWikiLink

inlineToWikiLink :: B.Inline -> Maybe WikiLink
inlineToWikiLink inl = do
  B.Link (_id, _class, otherAttrs) _is (url, tit) <- pure inl
  Left (_, wl) <- delineateLink (otherAttrs <> one ("title", tit)) url
  pure wl

-- | Return the various ways to link to this model route
--
-- Foo/Bar/Qux.md -> [[Qux]], [[Bar/Qux]], [[Foo/Bar/Qux]]
--
-- All possible combinations of Wikilink type use is automatically included.
allowedWikiLinks :: HasCallStack => ModelRoute -> NonEmpty (WikiLinkType, WikiLink)
allowedWikiLinks r =
  let wls = fmap WikiLink $ tailsNE $ wlParts r
      typs :: NonEmpty WikiLinkType = NE.fromList [minBound .. maxBound]
   in liftM2 (,) typs wls
  where
    wlParts =
      either (unRoute . lmlRouteCase) unRoute
        . modelRouteCase
    tailsNE =
      NE.fromList . mapMaybe nonEmpty . tails . toList

-------------------------
-- Parser
--------------------------

-- | A # prefix or suffix allows semantically distinct wikilinks
--
-- Typically called branching link or a tag link, when used with #.
data WikiLinkType
  = -- | [[Foo]]
    WikiLinkNormal
  | -- | [[Foo]]#
    WikiLinkBranch
  | -- | #[[Foo]]
    WikiLinkTag
  | -- | ![[Foo]]
    WikiLinkEmbed
  deriving (Eq, Show, Ord, Typeable, Data, Enum, Bounded)

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

wikilinkInline :: WikiLinkType -> WikiLink -> B.Inlines -> B.Inlines
wikilinkInline typ wl = B.linkWith attrs (wikilinkUrl wl) ""
  where
    attrs = ("", [], [(htmlAttr, show typ)])

-- | Like `Commonmark.Extensions.Wikilinks.wikilinkSpec` but Zettelkasten-friendly.
--
-- Compared with the official extension, this has two differences:
--
-- - Supports flipped inner text, eg: `[[Foo | some inner text]]`
-- - Supports neuron folgezettel, i.e.: #[[Foo]] or [[Foo]]#
wikilinkSpec ::
  (Monad m, CM.IsInline il, HasWikiLink il) =>
  CM.SyntaxSpec m il bl
wikilinkSpec =
  mempty
    { CM.syntaxInlineParsers =
        [ P.try $
            P.choice
              [ P.try (CT.symbol '#' *> pWikilink WikiLinkTag),
                P.try (CT.symbol '!' *> pWikilink WikiLinkEmbed),
                P.try (pWikilink WikiLinkBranch <* CT.symbol '#'),
                P.try (pWikilink WikiLinkNormal)
              ]
        ]
    }
  where
    pWikilink typ = do
      replicateM_ 2 $ CT.symbol '['
      P.notFollowedBy (CT.symbol '[')
      url <-
        CM.untokenize
          <$> many
            ( CT.satisfyTok
                ( \t ->
                    not (CT.hasType (CM.Symbol '|') t || CT.hasType (CM.Symbol ']') t)
                )
            )
      wl <- mkWikiLinkFromUrl url
      title <-
        M.optional $
          -- TODO: Should parse as inline so link text can be formatted?
          CM.untokenize
            <$> ( CT.symbol '|'
                    *> many (CT.satisfyTok (not . CT.hasType (CM.Symbol ']')))
                )
      replicateM_ 2 $ CT.symbol ']'
      return $ wikilink typ wl (fmap CM.str title)
