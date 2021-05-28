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
import Emanote.Model.Note (Note, noteDoc, noteRoute)
import Emanote.Route (LinkableLMLRoute, LinkableRoute)
import qualified Emanote.Route as R
import qualified Emanote.WikiLink as WL
import qualified Network.URI.Encode as UE
import qualified Text.Pandoc.Definition as B
import qualified Text.Pandoc.LinkContext as LC

type RelTarget = Either WL.WikiLink LinkableRoute

-- | A relation from a note to another note or static file.
data Rel = Rel
  { -- The note containing this relation
    _relFrom :: LinkableLMLRoute,
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

type RelIxs = '[LinkableLMLRoute, RelTarget]

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
          target <- parseRelTarget attrs url
          pure $ Rel (note ^. noteRoute) target ctx

-- | Parse a URL string
parseRelTarget :: [(Text, Text)] -> Text -> Maybe RelTarget
parseRelTarget attrs url = do
  guard $ not $ "://" `T.isInfixOf` url
  fmap (Left . snd) (WL.mkWikiLinkFromUrlAndAttrs attrs url)
    <|> fmap
      (Right . R.liftLinkableRoute . R.someLinkableLMLRouteCase)
      (R.mkLinkableLMLRouteFromFilePath $ UE.decode (toString url))
