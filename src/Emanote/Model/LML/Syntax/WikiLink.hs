{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Emanote.Model.LML.Syntax.WikiLink
  ( WikiLink,
    wikilinkSpec,
    mkWikiLinkFromUrlAndAttrs,
    allowedWikiLinks,
  )
where

import qualified Commonmark as CM
import qualified Commonmark.Pandoc as CP
import qualified Commonmark.TokParsers as CT
import Data.Data (Data)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Ema (Slug)
import qualified Ema
import Emanote.Route (LinkableRoute, R (unRoute), linkableLMLRouteCase, linkableRouteCase)
import qualified Text.Megaparsec as M
import qualified Text.Pandoc.Builder as B
import qualified Text.Parsec as P
import Text.Read (Read (readsPrec))

-- | Represents the "Foo" in [[Foo]]
--
-- As wiki links may contain multiple path components, it can also represent
-- [[Foo/Bar]], hence we use nonempty slug list.
newtype WikiLink = WikiLink {unWikiLink :: NonEmpty Slug}
  deriving (Eq, Show, Ord, Typeable, Data)

mkWikiLinkFromUrlAndAttrs :: [(Text, Text)] -> Text -> Maybe (WikiLinkType, WikiLink)
mkWikiLinkFromUrlAndAttrs (Map.fromList -> attrs) s = do
  wlType :: WikiLinkType <- readMaybe . toString <=< Map.lookup "title" $ attrs
  wl <- mkWikiLinkFromUrl s
  pure (wlType, wl)
  where
    mkWikiLinkFromUrl :: Text -> Maybe WikiLink
    mkWikiLinkFromUrl url = do
      -- TODO: Handle mailto: etc.
      guard $ not $ "://" `T.isInfixOf` url
      slugs <- nonEmpty $ Ema.decodeSlug <$> T.splitOn "/" url
      pure $ WikiLink slugs

-- | Return the various ways to link to this markdown route
--
-- Foo/Bar/Qux.md -> [[Qux]], [[Bar/Qux]], [[Foo/Bar/Qux]]
allowedWikiLinks :: LinkableRoute -> [WikiLink]
allowedWikiLinks =
  mapMaybe (fmap WikiLink . nonEmpty)
    . toList
    . NE.tails
    . wlParts
  where
    wlParts =
      either (unRoute . linkableLMLRouteCase) unRoute
        . linkableRouteCase

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
  deriving (Eq, Show, Ord, Typeable, Data)

instance Read WikiLinkType where
  readsPrec _ s
    | s == show WikiLinkNormal = [(WikiLinkNormal, "")]
    | s == show WikiLinkBranch = [(WikiLinkBranch, "")]
    | s == show WikiLinkTag = [(WikiLinkTag, "")]
    | s == show WikiLinkEmbed = [(WikiLinkEmbed, "")]
    | otherwise = []

class HasWikiLink il where
  wikilink :: WikiLinkType -> Text -> il -> il

instance HasWikiLink (CP.Cm b B.Inlines) where
  wikilink typ t il = CP.Cm $ B.link t (show typ) $ CP.unCm il

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
      title <-
        M.option url $
          CM.untokenize
            <$> ( CT.symbol '|'
                    *> many (CT.satisfyTok (not . CT.hasType (CM.Symbol ']')))
                )
      replicateM_ 2 $ CT.symbol ']'
      return $ wikilink typ url (CM.str title)
