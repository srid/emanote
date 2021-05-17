{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Emabook.Route.WikiLinkTarget where

import Data.Data (Data)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Data.Text as T
import Ema (Slug)
import qualified Ema
import Emabook.Route (Route (unRoute))
import Emabook.Route.Ext (Md)

-- | Represents the "Foo" in [[Foo]]
--
-- As wiki links may contain multiple path components, it can also represent
-- [[Foo/Bar]], hence we use nonempty slug list.
newtype WikiLinkTarget = WikiLinkTarget {unWikiLinkText :: NonEmpty Slug}
  deriving (Eq, Show, Ord, Data)

mkWikiLinkTargetFromUrl :: Text -> Maybe WikiLinkTarget
mkWikiLinkTargetFromUrl s = do
  guard $ not $ "://" `T.isInfixOf` s
  slugs <- nonEmpty $ Ema.decodeSlug <$> T.splitOn "/" s
  pure $ WikiLinkTarget slugs

-- | Return the various ways to link to this markdown route
--
-- Foo/Bar/Qux.md -> [[Qux]], [[Bar/Qux]], [[Foo/Bar/Qux]]
allowedWikiLinkTargets :: Route Md -> Set WikiLinkTarget
allowedWikiLinkTargets =
  Set.fromList . mapMaybe (fmap WikiLinkTarget . nonEmpty) . toList . NE.tails . unRoute
