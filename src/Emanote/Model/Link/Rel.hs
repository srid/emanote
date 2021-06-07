{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Emanote.Model.Link.Rel where

import Control.Lens.Operators as Lens ((^.))
import Control.Lens.TH (makeLenses)
import Data.IxSet.Typed (Indexable (..), IxSet, ixFun, ixList)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Emanote.Model.Note (Note, noteDoc, noteRoute)
import qualified Emanote.Pandoc.Markdown.Syntax.WikiLink as WL
import Emanote.Route (LinkableLMLRoute, LinkableRoute)
import qualified Emanote.Route as R
import qualified Network.URI.Encode as UE
import qualified Text.Pandoc.Definition as B
import qualified Text.Pandoc.LinkContext as LC

-- | A relation from one note to any other file (note or static file)
--
-- Target will remain unresolved in the `Rel`, and can be resolved at a latter
-- time (eg: during rendering).
data Rel = Rel
  { -- The note containing this relation
    _relFrom :: LinkableLMLRoute,
    -- The target of the relation (can be a note or anything)
    _relTo :: UnresolvedRelTarget,
    -- | The relation context in LML
    _relCtx :: [B.Block]
  }
  deriving (Show)

-- | A link target that has not been resolved (using model) yet.
type UnresolvedRelTarget = Either WL.WikiLink LinkableRoute

instance Eq Rel where
  (==) = (==) `on` (_relFrom &&& _relTo)

instance Ord Rel where
  (<=) = (<=) `on` (_relFrom &&& _relTo)

type RelIxs = '[LinkableLMLRoute, UnresolvedRelTarget]

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
          target <- parseUnresolvedRelTarget attrs url
          pure $ Rel (note ^. noteRoute) target ctx

unresolvedRelsTo :: LinkableRoute -> [UnresolvedRelTarget]
unresolvedRelsTo r =
  (Left <$> toList (WL.allowedWikiLinks r))
    <> [Right r]

-- | Parse a URL string for later resolution.
parseUnresolvedRelTarget :: [(Text, Text)] -> Text -> Maybe UnresolvedRelTarget
parseUnresolvedRelTarget attrs url = do
  guard $ not $ "://" `T.isInfixOf` url
  fmap (Left . snd) (WL.mkWikiLinkFromUrlAndAttrs attrs url)
    <|> fmap
      Right
      (R.mkLinkableRouteFromFilePath $ UE.decode (toString url))
