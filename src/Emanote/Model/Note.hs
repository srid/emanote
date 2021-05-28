{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.Model.Note where

import Control.Lens.Operators as Lens ((^.))
import Control.Lens.TH (makeLenses)
import qualified Data.Aeson as Aeson
import Data.IxSet.Typed (Indexable (..), IxSet, ixFun, ixList)
import qualified Ema.Helper.Markdown as Markdown
import qualified Emanote.PandocUtil as PandocUtil
import qualified Emanote.Route as R
import Emanote.Route.Linkable
import qualified Emanote.WikiLink as WL
import Text.Pandoc.Definition (Pandoc (..))

data Note = Note
  { _noteDoc :: Pandoc,
    _noteMeta :: Aeson.Value,
    _noteRoute :: LinkableLMLRoute
  }
  deriving (Eq, Ord, Show, Generic, Aeson.ToJSON)

-- | All possible wiki-links that refer to this note.
noteSelfRefs :: Note -> [WL.WikiLink]
noteSelfRefs =
  WL.allowedWikiLinks
    . (liftLinkableRoute . someLinkableLMLRouteCase)
    . _noteRoute

type NoteIxs = '[LinkableLMLRoute, WL.WikiLink]

type IxNote = IxSet NoteIxs Note

instance Indexable NoteIxs Note where
  indices =
    ixList
      (ixFun $ one . _noteRoute)
      (ixFun noteSelfRefs)

makeLenses ''Note

noteTitle :: Note -> Text
noteTitle note =
  fromMaybe (R.routeBaseName . someLinkableLMLRouteCase $ note ^. noteRoute) $
    PandocUtil.getPandocTitle $ note ^. noteDoc

parseNote :: MonadIO m => LinkableLMLRoute -> FilePath -> m (Either Text Note)
parseNote r fp = do
  !s <- readFileText fp
  pure $ do
    (mMeta, doc) <- parseMarkdown fp s
    pure $ Note doc (fromMaybe Aeson.Null mMeta) r
  where
    parseMarkdown =
      Markdown.parseMarkdownWithFrontMatter @Aeson.Value $
        WL.wikilinkSpec <> Markdown.fullMarkdownSpec
