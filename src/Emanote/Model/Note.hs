{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.Model.Note where

import Control.Lens.TH (makeLenses)
import qualified Data.Aeson as Aeson
import Data.IxSet.Typed (Indexable (..), IxSet, ixFun, ixList)
import qualified Data.IxSet.Typed as Ix
import Ema (Slug)
import qualified Emanote.Pandoc.Markdown.Parser as Markdown
import qualified Emanote.Pandoc.Markdown.Syntax.WikiLink as WL
import Emanote.Route (R)
import qualified Emanote.Route as R
import Relude.Extra.Map (StaticMap (lookup))
import Text.Pandoc.Definition (Pandoc (..))
import qualified Text.Pandoc.Definition as B

data Note = Note
  { _noteDoc :: Pandoc,
    _noteMeta :: Aeson.Value,
    _noteRoute :: R.LinkableLMLRoute
  }
  deriving (Eq, Ord, Show, Generic, Aeson.ToJSON)

newtype RAncestor = RAncestor {unRAncestor :: R 'R.Folder}
  deriving (Eq, Ord, Show, Generic, Aeson.ToJSON)

type NoteIxs =
  '[ R.LinkableLMLRoute,
     -- Allowed ways to wiki-link to this note.
     WL.WikiLink,
     -- HTML route for this note
     R 'R.Html,
     -- Parent folder (unless a root file).
     -- TODO: Index parent folder's allowed wikilinks, somehow, to support
     -- linking to them.
     R 'R.Folder,
     -- All ancestors
     RAncestor,
     -- Tag
     Text,
     -- "slug" alias
     Slug
   ]

type IxNote = IxSet NoteIxs Note

instance Indexable NoteIxs Note where
  indices =
    ixList
      (ixFun $ one . _noteRoute)
      (ixFun noteSelfRefs)
      (ixFun $ one . noteHtmlRoute)
      (ixFun $ maybeToList . noteParent)
      (ixFun $ maybe [] (toList . fmap RAncestor . R.routeInits) . noteParent)
      (ixFun noteTags)
      (ixFun $ maybeToList . noteSlug)
    where
      noteParent = R.routeParent . R.linkableLMLRouteCase . _noteRoute

-- | All possible wiki-links that refer to this note.
noteSelfRefs :: Note -> [WL.WikiLink]
noteSelfRefs =
  WL.allowedWikiLinks
    . (R.liftLinkableRoute . R.linkableLMLRouteCase)
    . _noteRoute

noteTags :: Note -> [Text]
noteTags =
  lookupAeson mempty (one "tags") . _noteMeta

noteTitle :: Note -> Text
noteTitle Note {..} =
  fromMaybe (R.routeBaseName . R.linkableLMLRouteCase $ _noteRoute) $
    getPandocTitle _noteDoc
  where
    getPandocTitle :: Pandoc -> Maybe Text
    getPandocTitle =
      fmap Markdown.plainify . getPandocH1
      where
        getPandocH1 :: Pandoc -> Maybe [B.Inline]
        getPandocH1 (Pandoc _ (B.Header 1 _ inlines : _rest)) =
          Just inlines
        getPandocH1 _ =
          Nothing

noteSlug :: Note -> Maybe Slug
noteSlug =
  lookupAeson Nothing (one "slug") . _noteMeta

-- | The HTML route intended by user for this note.
noteHtmlRoute :: Note -> R 'R.Html
noteHtmlRoute note@Note {..} =
  -- Favour slug if one exixts, otherwise use the full path.
  case noteSlug note of
    Nothing ->
      coerce $ R.linkableLMLRouteCase _noteRoute
    Just slug ->
      R.mkRouteFromSlug slug

-- | Does the given folder have any notes?
hasNotes :: R 'R.Folder -> IxNote -> Bool
hasNotes r =
  not . Ix.null . Ix.getEQ r

singleNote :: HasCallStack => [Note] -> Maybe Note
singleNote ns = do
  res@(x :| xs) <- nonEmpty ns
  if null xs
    then pure x
    else error $ "Ambiguous notes: " <> show (_noteRoute <$> res)

lookupNotesByHtmlRoute :: R 'R.Html -> IxNote -> [Note]
lookupNotesByHtmlRoute htmlRoute =
  Ix.toList . Ix.getEQ htmlRoute

lookupNotesByRoute :: R.LinkableLMLRoute -> IxNote -> [Note]
lookupNotesByRoute htmlRoute =
  Ix.toList . Ix.getEQ htmlRoute

lookupFolderWithNotes :: R 'R.Folder -> IxNote -> Maybe Note
lookupFolderWithNotes r ns = do
  guard $ hasNotes (coerce r) ns
  let placeHolder =
        Pandoc mempty $
          one $
            B.Plain
              [ B.Str
                  "Placeholder: To add content here, write to file: ",
                B.Code B.nullAttr $ toText (R.encodeRoute r) <> ".md"
              ]
      folderMdR = R.liftLinkableLMLRoute @('R.LMLType 'R.Md) . coerce $ r
  pure $ mkEmptyNoteWith folderMdR placeHolder
  where
    mkEmptyNoteWith someR doc =
      Note doc Aeson.Null someR

parseNote :: MonadIO m => R.LinkableLMLRoute -> FilePath -> m (Either Text Note)
parseNote r fp = do
  !s <- readFileText fp
  pure $ do
    (mMeta, doc) <- Markdown.parseMarkdown fp s
    let meta = fromMaybe Aeson.Null mMeta
    pure $ Note doc meta r

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

makeLenses ''Note
