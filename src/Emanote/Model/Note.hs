{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Emanote.Model.Note where

import Control.Monad.Except
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Optics qualified as AO
import Data.Default (def)
import Data.IxSet.Typed (Indexable (..), IxSet, ixFun, ixList)
import Data.IxSet.Typed qualified as Ix
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Emanote.Model.SData qualified as SData
import Emanote.Model.Title qualified as Tit
import Emanote.Pandoc.Markdown.Parser qualified as Markdown
import Emanote.Pandoc.Markdown.Syntax.HashTag qualified as HT
import Emanote.Pandoc.Markdown.Syntax.WikiLink qualified as WL
import Emanote.Route (FileType (Folder), R)
import Emanote.Route qualified as R
import Network.URI.Slug (Slug)
import Optics.Core ((%), (.~))
import Optics.TH (makeLenses)
import Relude
import System.Directory (doesFileExist)
import System.FilePath (takeExtension, (</>))
import Text.Pandoc (PandocError, runIO)
import Text.Pandoc.Builder qualified as B
import Text.Pandoc.Definition (Pandoc (..))
import Text.Pandoc.Filter qualified as PF

data Note = Note
  { _noteRoute :: R.LMLRoute,
    _noteDoc :: Pandoc,
    _noteMeta :: Aeson.Value,
    _noteTitle :: Tit.Title
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Aeson.ToJSON)

newtype RAncestor = RAncestor {unRAncestor :: R 'R.Folder}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Aeson.ToJSON)

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
  fmap R.unRoute $ R.mkRouteFromFilePath @_ @'R.AnyExt $ toString slugPath

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

data NoteParseError = NPEParse Text | NPEPandoc PandocError | NPEFilter Text
  deriving stock (Show, Generic)
  deriving anyclass (Exception)

parseNote ::
  forall m.
  (MonadError NoteParseError m, MonadIO m) =>
  FilePath ->
  R.LMLRoute ->
  FilePath ->
  Text ->
  m Note
parseNote pluginBaseDir r fp s = do
  (withAesonDefault defaultFrontMatter -> frontmatter, doc') <-
    liftEither . first NPEParse $ Markdown.parseMarkdown fp s
  filters <-
    fmap catMaybes . traverse mkLuaFilter $
      lookupAeson @[FilePath] mempty ("pandoc" :| ["filters"]) frontmatter
  doc <- if null filters then pure doc' else runPandocFilters filters doc'
  let meta = addTagsFromBody doc frontmatter
  pure $ Note r doc meta (queryNoteTitle r doc meta)
  where
    withAesonDefault default_ mv =
      fromMaybe default_ mv
        `SData.mergeAeson` default_
    defaultFrontMatter =
      Aeson.toJSON $ Map.fromList @Text @[Text] $ one ("tags", [])
    -- Merge frontmatter tags with inline tags in Pandoc document.
    -- DESIGN: In retrospect, this is like a Pandoc lua filter?
    addTagsFromBody doc frontmatter =
      frontmatter
        & AO.key "tags" % AO._Array
        .~ ( fromList . fmap Aeson.toJSON $
               nub $
                 lookupAeson @[HT.Tag] mempty (one "tags") frontmatter
                   <> HT.inlineTagsInPandoc doc
           )
    mkLuaFilter ((pluginBaseDir </>) -> relPath) = do
      if takeExtension relPath == ".lua"
        then do
          liftIO (doesFileExist relPath) >>= \case
            True -> pure $ Just $ PF.LuaFilter relPath
            False -> throwError $ NPEFilter $ toText $ relPath <> " is missing"
        else pure Nothing
    runPandocFilters filters doc = do
      liftEither . first NPEPandoc =<< liftIO (runIO $ PF.applyFilters def filters ["markdown"] doc)

-- TODO: Use https://hackage.haskell.org/package/lens-aeson
lookupAeson :: forall a. Aeson.FromJSON a => a -> NonEmpty Text -> Aeson.Value -> a
lookupAeson x (k :| ks) meta =
  fromMaybe x $ do
    Aeson.Object obj <- pure meta
    val <- KM.lookup (fromString . toString $ k) obj
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
      Aeson.object [(fromString . toString) x Aeson..= oneAesonText (toList xs) v]

makeLenses ''Note
