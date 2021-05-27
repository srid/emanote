{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Emanote.Model.Rel where

import Control.Lens.Operators as Lens ((^.))
import Control.Lens.TH (makeLenses)
import Data.IxSet.Typed (Indexable (..), IxSet, ixFun, ixList)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.WorldPeace.Union (openUnionLift)
import Emanote.Model.Note (Note, noteDoc, noteRoute)
import Emanote.Route.SomeRoute (SomeLMLRoute, SomeRoute, mkLmlRouteFromFilePath, someLMLRouteCase)
import qualified Emanote.Route.WikiLinkTarget as WL
import qualified Text.Pandoc.Definition as B
import qualified Text.Pandoc.LinkContext as LC

type RelTarget = Either WL.WikiLinkTarget SomeRoute

-- | A relation from a note to another note or static file.
data Rel = Rel
  { -- The note containing this relation
    _relFrom :: SomeLMLRoute,
    -- The target of the relation (can be a note or anything)
    _relTo :: RelTarget,
    -- | The relation context in LML
    _relCtx :: [B.Block]
  }
  deriving (Show)

instance Eq Rel where
  (==) = (==) `on` (_relFrom &&& _relTo)

instance Ord Rel where
  (<=) = (<=) `on` (_relFrom &&& _relTo)

type RelIxs = '[SomeLMLRoute, RelTarget]

type IxRel = IxSet RelIxs Rel

instance Indexable RelIxs Rel where
  indices =
    ixList
      (ixFun $ one . _relFrom)
      (ixFun $ one . _relTo)

makeLenses ''Rel

extractRels :: Note -> [Rel]
extractRels note =
  extractLinks . LC.queryLinksWithContext $ note ^. noteDoc
  where
    extractLinks :: Map Text (NonEmpty ([(Text, Text)], [B.Block])) -> [Rel]
    extractLinks m =
      flip concatMap (Map.toList m) $ \(url, instances) -> do
        flip mapMaybe (toList instances) $ \(attrs, ctx) -> do
          target <- parseRelTarget url
          pure $ Rel (note ^. noteRoute) target ctx

-- | Parse a URL string
parseRelTarget :: Text -> Maybe RelTarget
parseRelTarget url = do
  guard $ not $ "://" `T.isInfixOf` url
  -- NOTE: wiki link parsing must come **last**, as it catches every relative
  -- URL.
  -- TODO: Use mkAnyExtRouteFromFilePath to catch static file paths, but only if they exist
  -- BUT this can only be done when parsing wiki-link only in wiki-links (not regular links)
  -- So make link-context return entire pandoc link node, and parse wiki-link identifiers.
  -- DO the same for PandocUtil.rewriteLinks
  -- THEN during resolution:
  -- Make wiki link parser look for all sources including static sources (and dirs).
  -- See the TODO in modelLookupRouteByWikiLink
  fmap
    (Right . openUnionLift . someLMLRouteCase)
    (mkLmlRouteFromFilePath . toString $ url)
    <|> fmap Left (WL.mkWikiLinkTargetFromUrl url)
