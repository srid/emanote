{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.Model.Note where

import Control.Lens.Operators ((.~))
import Control.Lens.TH (makeLenses)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Lens as A
import Data.IxSet.Typed (Indexable (..), IxSet, ixFun, ixList)
import qualified Data.IxSet.Typed as Ix
import qualified Data.Map.Strict as Map
import Ema (Slug)
import qualified Emanote.Model.SData as SData
import qualified Emanote.Pandoc.Markdown.Parser as Markdown
import qualified Emanote.Pandoc.Markdown.Syntax.HashTag as HT
import qualified Emanote.Pandoc.Markdown.Syntax.WikiLink as WL
import Emanote.Route (FileType (Folder), R)
import qualified Emanote.Route as R
import Heist.Extra.Splices.Pandoc.Render (plainify)
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
     -- Ancestor folder routes
     RAncestor,
     -- Parent folder
     R 'R.Folder,
     -- Tag
     HT.Tag,
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
      (ixFun noteAncestors)
      (ixFun $ maybeToList . noteParent)
      (ixFun noteTags)
      (ixFun $ maybeToList . noteSlug)

-- | All possible wiki-links that refer to this note.
noteSelfRefs :: Note -> [WL.WikiLink]
noteSelfRefs =
  WL.allowedWikiLinks
    . (R.liftLinkableRoute . R.linkableLMLRouteCase)
    . _noteRoute

noteAncestors :: Note -> [RAncestor]
noteAncestors =
  maybe [] (toList . fmap RAncestor . R.routeInits) . noteParent

noteParent :: Note -> Maybe (R 'R.Folder)
noteParent = R.routeParent . R.linkableLMLRouteCase . _noteRoute

hasChildNotes :: R 'Folder -> IxNote -> Bool
hasChildNotes r =
  not . Ix.null . Ix.getEQ r

noteTags :: Note -> [HT.Tag]
noteTags =
  fmap HT.Tag . lookupAeson mempty (one "tags") . _noteMeta

noteTitle :: Note -> Text
noteTitle Note {..} =
  fromMaybe (R.routeBaseName . R.linkableLMLRouteCase $ _noteRoute) $
    getPandocTitle _noteDoc
  where
    getPandocTitle :: Pandoc -> Maybe Text
    getPandocTitle =
      fmap plainify . getPandocH1
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
  -- Favour slug if one exists, otherwise use the full path.
  case noteSlug note of
    Nothing ->
      coerce $ R.linkableLMLRouteCase _noteRoute
    Just slug ->
      R.mkRouteFromSlug slug

notePath :: Note -> FilePath
notePath =
  R.encodeRoute . R.linkableLMLRouteCase . _noteRoute

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

placeHolderNote :: R.LinkableLMLRoute -> Note
placeHolderNote r =
  let placeHolder =
        B.Plain
          [ B.Str
              "To add content here, create a file named: ",
            B.Code B.nullAttr $ toText (R.encodeRoute $ R.linkableLMLRouteCase r)
          ]
   in mkEmptyNoteWith r placeHolder

mkEmptyNoteWith :: R.LinkableLMLRoute -> B.Block -> Note
mkEmptyNoteWith someR (Pandoc mempty . one -> doc) =
  Note doc Aeson.Null someR

parseNote :: MonadIO m => R.LinkableLMLRoute -> FilePath -> m (Either Text Note)
parseNote r fp = do
  !s <- readFileText fp
  pure $ do
    (withAesonDefault defaultFrontMatter -> frontmatter, doc) <-
      Markdown.parseMarkdown fp s
    let meta =
          frontmatter
            -- Merge frontmatter tags with inline tags in Pandoc document.
            & A.key "tags" . A._Array
              .~ ( fromList . fmap Aeson.toJSON $
                     lookupAeson mempty (one "tags") frontmatter
                       <> HT.inlineTagsInPandoc doc
                 )
    pure $ Note doc meta r
  where
    withAesonDefault def mv =
      fromMaybe def mv
        `SData.mergeAeson` def
    defaultFrontMatter =
      Aeson.toJSON $ Map.fromList @Text @[Text] $ one ("tags", [])

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
