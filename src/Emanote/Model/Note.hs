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
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Ema (Slug)
import qualified Emanote.Model.SData as SData
import qualified Emanote.Model.Title as Tit
import qualified Emanote.Pandoc.Markdown.Parser as Markdown
import qualified Emanote.Pandoc.Markdown.Syntax.HashTag as HT
import qualified Emanote.Pandoc.Markdown.Syntax.WikiLink as WL
import Emanote.Route (FileType (Folder), R)
import qualified Emanote.Route as R
import Relude
import Relude.Extra.Map (StaticMap (lookup))
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))

data Note = Note
  { _noteRoute :: R.LMLRoute,
    _noteDoc :: Pandoc,
    _noteMeta :: Aeson.Value,
    _noteTitle :: Tit.Title
  }
  deriving (Eq, Ord, Show, Generic, Aeson.ToJSON)

newtype RAncestor = RAncestor {unRAncestor :: R 'R.Folder}
  deriving (Eq, Ord, Show, Generic, Aeson.ToJSON)

type NoteIxs =
  '[ -- Route to this note
     R.LMLRoute,
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
     -- Alias route for this note. Can be "foo" or "foo/bar".
     NonEmpty Slug
   ]

type IxNote = IxSet NoteIxs Note

instance Indexable NoteIxs Note where
  indices =
    ixList
      (ixFun $ one . _noteRoute)
      (ixFun $ toList . noteSelfRefs)
      (ixFun $ one . noteHtmlRoute)
      (ixFun noteAncestors)
      (ixFun $ maybeToList . noteParent)
      (ixFun noteTags)
      (ixFun $ maybeToList . noteSlug)

-- | All possible wiki-links that refer to this note.
noteSelfRefs :: Note -> NonEmpty WL.WikiLink
noteSelfRefs =
  routeSelfRefs
    . _noteRoute
  where
    routeSelfRefs :: R.LMLRoute -> NonEmpty WL.WikiLink
    routeSelfRefs =
      fmap snd
        . WL.allowedWikiLinks
        . R.lmlRouteCase

noteAncestors :: Note -> [RAncestor]
noteAncestors =
  maybe [] (toList . fmap RAncestor . R.routeInits) . noteParent

noteParent :: Note -> Maybe (R 'R.Folder)
noteParent = R.routeParent . R.lmlRouteCase . _noteRoute

hasChildNotes :: R 'Folder -> IxNote -> Bool
hasChildNotes r =
  not . Ix.null . Ix.getEQ r

noteTags :: Note -> [HT.Tag]
noteTags =
  fmap HT.Tag . fromMaybe mempty . lookupMeta (one "tags")

noteSlug :: Note -> Maybe (NonEmpty Slug)
noteSlug note = do
  slugPath :: Text <- lookupMeta (one "slug") note
  fmap R.unRoute $ R.mkRouteFromFilePath @'R.AnyExt $ toString slugPath

lookupMeta :: Aeson.FromJSON a => NonEmpty Text -> Note -> Maybe a
lookupMeta k =
  lookupAeson Nothing k . _noteMeta

queryNoteTitle :: R.LMLRoute -> Pandoc -> Aeson.Value -> Tit.Title
queryNoteTitle r doc meta =
  let yamlNoteTitle = fromString <$> lookupAeson Nothing (one "title") meta
      fileNameTitle = Tit.fromRoute r
      notePandocTitle = getPandocTitle doc
   in fromMaybe fileNameTitle $ yamlNoteTitle <|> notePandocTitle
  where
    getPandocTitle :: Pandoc -> Maybe Tit.Title
    getPandocTitle =
      fmap Tit.fromInlines . getPandocH1
      where
        getPandocH1 :: Pandoc -> Maybe [B.Inline]
        getPandocH1 (Pandoc _ (B.Header 1 _ inlines : _rest)) =
          Just inlines
        getPandocH1 _ =
          Nothing

-- | The HTML route intended by user for this note.
noteHtmlRoute :: Note -> R 'R.Html
noteHtmlRoute note@Note {..} =
  -- Favour slug if one exists, otherwise use the full path.
  case noteSlug note of
    Nothing ->
      coerce $ R.lmlRouteCase _noteRoute
    Just slugs ->
      R.mkRouteFromSlugs slugs

lookupNotesByHtmlRoute :: R 'R.Html -> IxNote -> [Note]
lookupNotesByHtmlRoute htmlRoute =
  Ix.toList . Ix.getEQ htmlRoute

lookupNotesByRoute :: HasCallStack => R.LMLRoute -> IxNote -> Maybe Note
lookupNotesByRoute r ix = do
  res <- nonEmpty $ Ix.toList $ Ix.getEQ r ix
  case res of
    note :| [] -> pure note
    _ -> error $ "ambiguous notes for route " <> show r

ancestorPlaceholderNote :: R.LMLRoute -> Note
ancestorPlaceholderNote r =
  let placeHolder =
        [ folderListingQuery,
          -- TODO: Ideally, we should use semantic tags, like <aside> (rather
          -- than <div>), to render these non-relevant content.
          B.Div (cls "emanote:placeholder-message") . one . B.Para $
            [ B.Str
                "Note: To override the auto-generated content here, create a file named: ",
              B.Span (cls "font-mono text-sm") $ one $ B.Str $ toText (R.encodeRoute $ R.lmlRouteCase r)
            ]
        ]
   in mkEmptyNoteWith r placeHolder
  where
    folderListingQuery =
      B.CodeBlock (cls "query") "path:./*"
    cls x =
      ("", one x, mempty) :: B.Attr

missingNote :: R.LMLRoute -> Text -> Note
missingNote route404 urlPath =
  mkEmptyNoteWith route404 $
    one $
      B.Para
        [ B.Str "No note has the URL ",
          B.Code B.nullAttr $ "/" <> urlPath,
          B.Str ". You may create a Markdown file with that name."
        ]

ambiguousNoteURL :: FilePath -> NonEmpty R.LMLRoute -> Note
ambiguousNoteURL urlPath rs =
  mkEmptyNoteWith (head rs) $
    [ B.Para
        [ B.Str "The URL ",
          B.Code B.nullAttr $ toText urlPath,
          B.Str " is ambiguous, as more than one note (see list below) use it. To fix this, specify a different slug for these notes:"
        ]
    ]
      <> one candidates
  where
    candidates :: B.Block
    candidates =
      B.BulletList $
        toList rs <&> \(R.lmlRouteCase -> r) ->
          [ B.Plain $ one $ B.Str "  ",
            B.Plain $ one $ B.Code B.nullAttr $ show r
          ]

mkEmptyNoteWith :: R.LMLRoute -> [B.Block] -> Note
mkEmptyNoteWith someR (Pandoc mempty -> doc) =
  Note someR doc meta (queryNoteTitle someR doc meta)
  where
    meta = Aeson.Null

parseNote :: R.LMLRoute -> FilePath -> Text -> Either Text Note
parseNote r fp s = do
  (withAesonDefault defaultFrontMatter -> frontmatter, doc) <-
    Markdown.parseMarkdown fp s
  let meta =
        frontmatter
          -- Merge frontmatter tags with inline tags in Pandoc document.
          & A.key "tags" . A._Array
            .~ ( fromList . fmap Aeson.toJSON $
                   nub $
                     lookupAeson @[HT.Tag] mempty (one "tags") frontmatter
                       <> HT.inlineTagsInPandoc doc
               )
  pure $ Note r doc meta (queryNoteTitle r doc meta)
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

oneAesonText :: [Text] -> Text -> Aeson.Value
oneAesonText k v =
  case nonEmpty k of
    Nothing ->
      Aeson.String v
    Just (x :| xs) ->
      Aeson.object [x Aeson..= oneAesonText (toList xs) v]

makeLenses ''Note
