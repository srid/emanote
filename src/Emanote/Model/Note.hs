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
import qualified Data.IxSet.Typed as Ix
import Ema (Slug)
import qualified Ema.Helper.Markdown as Markdown
import qualified Emanote.Prelude as EP
import Emanote.Route (R)
import qualified Emanote.Route as R
import qualified Emanote.WikiLink as WL
import Relude.Extra.Map (StaticMap (lookup))
import Text.Pandoc.Definition (Pandoc (..))

data Note = Note
  { _noteDoc :: Pandoc,
    _noteMeta :: Aeson.Value,
    _noteTags :: [Text],
    _noteRoute :: R.LinkableLMLRoute,
    -- | Custom slug set in frontmatter if any. Overrides _noteRoute for
    -- determining the URL.
    _noteSlug :: Maybe Slug
  }
  deriving (Eq, Ord, Show, Generic, Aeson.ToJSON)

-- | All possible wiki-links that refer to this note.
noteSelfRefs :: Note -> [WL.WikiLink]
noteSelfRefs =
  WL.allowedWikiLinks
    . (R.liftLinkableRoute . R.someLinkableLMLRouteCase)
    . _noteRoute

type NoteIxs = '[R.LinkableLMLRoute, WL.WikiLink, Text, Slug]

type IxNote = IxSet NoteIxs Note

instance Indexable NoteIxs Note where
  indices =
    ixList
      (ixFun $ one . _noteRoute)
      (ixFun noteSelfRefs)
      (ixFun _noteTags)
      (ixFun $ maybeToList . _noteSlug)

makeLenses ''Note

noteTitle :: Note -> Text
noteTitle note =
  fromMaybe (R.routeBaseName . R.someLinkableLMLRouteCase $ note ^. noteRoute) $
    EP.getPandocTitle $ note ^. noteDoc

noteHtmlRoute :: Note -> R 'R.Html
noteHtmlRoute note =
  -- Favour slug if one exixts, otherwise use the full path.
  case lookupAeson @(Maybe Slug) Nothing (one "slug") (note ^. noteMeta) of
    Nothing ->
      coerce $ R.someLinkableLMLRouteCase (note ^. noteRoute)
    Just slug ->
      R.mkRouteFromSlug slug

-- | TODO: Ditch this in favour of direct indexing in html route.
lookupNote :: R 'R.Html -> IxNote -> Maybe Note
lookupNote htmlRoute ns =
  (Ix.getOne . Ix.getEQ (R.liftLinkableLMLRoute mdRoute)) ns
    <|> (mSlug >>= \slug -> (Ix.getOne . Ix.getEQ slug) ns)
  where
    mSlug :: Maybe Slug = do
      slug :| [] <- pure $ R.unRoute htmlRoute
      pure slug
    mdRoute :: R ('R.LMLType 'R.Md) =
      coerce htmlRoute

parseNote :: MonadIO m => R.LinkableLMLRoute -> FilePath -> m (Either Text Note)
parseNote r fp = do
  !s <- readFileText fp
  pure $ do
    (mMeta, doc) <- parseMarkdown fp s
    let meta = fromMaybe Aeson.Null mMeta
        tags = lookupAeson [] (one "tags") meta
        mSlug = lookupAeson Nothing (one "slug") meta
    pure $ Note doc meta tags r mSlug
  where
    parseMarkdown =
      Markdown.parseMarkdownWithFrontMatter @Aeson.Value $
        WL.wikilinkSpec <> Markdown.fullMarkdownSpec

-- TODO: Use https://hackage.haskell.org/package/lens-aeson
lookupAeson :: forall a. Aeson.FromJSON a => a -> NonEmpty Text -> Aeson.Value -> a
lookupAeson x (k :| ks) meta =
  fromMaybe x $ do
    Aeson.Object obj <- pure meta
    val <- lookup k obj
    case nonEmpty ks of
      Nothing -> resultToMaybe $ Aeson.fromJSON val
      Just ks' -> pure $ lookupAeson x ks' val
  where
    resultToMaybe :: Aeson.Result b -> Maybe b
    resultToMaybe = \case
      Aeson.Error _ -> Nothing
      Aeson.Success b -> pure b
