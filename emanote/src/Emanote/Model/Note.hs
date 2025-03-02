{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Emanote.Model.Note where

import Commonmark.Extensions.WikiLink qualified as WL
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Writer (MonadWriter (tell), WriterT, runWriterT)
import Data.Aeson qualified as Aeson
import Data.Aeson.Optics qualified as AO
import Data.Char (isDigit)
import Data.Default (Default (def))
import Data.IxSet.Typed (Indexable (..), IxSet, ixFun, ixList)
import Data.IxSet.Typed qualified as Ix
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Calendar (toGregorian)
import Emanote.Model.Calendar.Parser qualified as Calendar
import Emanote.Model.Note.Filter (applyPandocFilters)
import Emanote.Model.SData qualified as SData
import Emanote.Model.Title qualified as Tit
import Emanote.Pandoc.BuiltinFilters (preparePandoc)
import Emanote.Pandoc.Markdown.Parser qualified as Markdown
import Emanote.Pandoc.Markdown.Syntax.HashTag qualified as HT
import Emanote.Route qualified as R
import Emanote.Route.Ext (FileType (Folder))
import Emanote.Route.R (R)
import Emanote.Source.Loc (Loc)
import Network.URI.Slug (Slug)
import Optics.Core ((%), (.~))
import Optics.TH (makeLenses)
import Relude
import System.FilePath (takeFileName, (</>))
import Text.Pandoc (readerExtensions, runPure)
import Text.Pandoc.Builder qualified as B
import Text.Pandoc.Definition (Pandoc (..))
import Text.Pandoc.Extensions
import Text.Pandoc.Readers.Org (readOrg)
import Text.Pandoc.Scripting (ScriptingEngine)
import Text.Pandoc.Walk qualified as W
import Text.Parsec qualified as P
import Text.Printf (printf)
import UnliftIO.Directory (doesPathExist)

data Feed = Feed
  { _feedEnable :: Bool
  , _feedTitle :: Maybe Text
  , _feedLimit :: Maybe Word
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data Note = Note
  { _noteRoute :: R.LMLRoute
  , _noteSource :: Maybe (Loc, FilePath)
  -- ^ The layer from which this note came. Nothing if the note was auto-generated.
  , _noteDoc :: Pandoc
  , _noteMeta :: Aeson.Value
  , _noteTitle :: Tit.Title
  , _noteErrors :: [Text]
  , _noteFeed :: Maybe Feed
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Aeson.ToJSON)

newtype RAncestor = RAncestor {unRAncestor :: R 'R.Folder}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Aeson.ToJSON)

type NoteIxs =
  '[ -- Route to this note
     R.LMLRoute
   , -- Allowed ways to wiki-link to this note.
     WL.WikiLink
   , -- HTML route for this note
     R 'R.Html
   , -- XML route for this note
     R 'R.Xml
   , -- Ancestor folder routes
     RAncestor
   , -- Parent folder
     Maybe (R 'R.Folder)
   , -- Tag
     HT.Tag
   , -- Alias route for this note. Can be "foo" or "foo/bar".
     NonEmpty Slug
   ]

type IxNote = IxSet NoteIxs Note

instance Indexable NoteIxs Note where
  indices =
    ixList
      (ixFun $ one . _noteRoute)
      (ixFun $ toList . noteSelfRefs)
      (ixFun $ one . noteHtmlRoute)
      (ixFun $ maybeToList . noteXmlRoute)
      (ixFun noteAncestors)
      (ixFun $ one . noteParent)
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
        . R.withLmlRoute (WL.allowedWikiLinks . R.unRoute)

noteAncestors :: Note -> [RAncestor]
noteAncestors =
  maybe [] (toList . fmap RAncestor . R.routeInits) . noteParent

noteParent :: Note -> Maybe (R 'R.Folder)
noteParent = R.withLmlRoute R.routeParent . _noteRoute

hasChildNotes :: R 'Folder -> IxNote -> Bool
hasChildNotes r =
  not . Ix.null . Ix.getEQ (Just r)

noteTags :: Note -> [HT.Tag]
noteTags =
  fmap HT.Tag . maybeToMonoid . lookupMeta (one "tags")

noteSlug :: Note -> Maybe (NonEmpty Slug)
noteSlug note = do
  slugPath :: Text <- lookupMeta (one "slug") note
  fmap R.unRoute $ R.mkRouteFromFilePath @_ @'R.AnyExt $ toString slugPath

lookupMeta :: (Aeson.FromJSON a) => NonEmpty Text -> Note -> Maybe a
lookupMeta k =
  SData.lookupAeson Nothing k . _noteMeta

noteHasFeed :: Note -> Bool
noteHasFeed = maybe False _feedEnable . _noteFeed

queryNoteFeed :: Aeson.Value -> Maybe Feed
queryNoteFeed meta = do
  feed <- SData.lookupAeson Nothing (one "feed") meta
  let title = SData.lookupAeson Nothing (one "title") feed
  let enable = SData.lookupAeson False (one "enable") feed
  let feedLimit = SData.lookupAeson Nothing (one "limit") feed
  pure $ Feed enable title feedLimit

queryNoteTitle :: R.LMLRoute -> Pandoc -> Aeson.Value -> (Pandoc, Tit.Title)
queryNoteTitle r doc meta =
  let yamlNoteTitle = fromString <$> SData.lookupAeson Nothing (one "title") meta
      fileNameTitle = Tit.fromRoute r
      notePandocTitle = do
        case r of
          R.LMLRoute_Md _ ->
            getPandocTitle doc
          R.LMLRoute_Org _ ->
            getPandocMetaTitle doc
   in fromMaybe (doc, fileNameTitle)
        $ fmap (doc,) yamlNoteTitle
        <|> fmap (withoutH1 doc,) notePandocTitle
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
    getPandocMetaTitle :: Pandoc -> Maybe Tit.Title
    getPandocMetaTitle (Pandoc docMeta _) = do
      B.MetaInlines inlines <- B.lookupMeta "title" docMeta
      pure $ Tit.fromInlines inlines
    withoutH1 :: B.Pandoc -> B.Pandoc
    withoutH1 (B.Pandoc m (B.Header 1 _ _ : rest)) =
      B.Pandoc m rest
    withoutH1 x =
      x

-- | The xml route intended by user for this note.
noteXmlRoute :: Note -> Maybe (R 'R.Xml)
noteXmlRoute note
  | noteHasFeed note = Just (coerce $ noteHtmlRoute note)
  | otherwise = Nothing

-- | The HTML route intended by user for this note.
noteHtmlRoute :: Note -> R 'R.Html
noteHtmlRoute note@Note {..} =
  -- Favour slug if one exists, otherwise use the full path.
  case noteSlug note of
    Nothing ->
      R.withLmlRoute coerce _noteRoute
    Just slugs ->
      R.mkRouteFromSlugs slugs

lookupNotesByHtmlRoute :: R 'R.Html -> IxNote -> [Note]
lookupNotesByHtmlRoute htmlRoute =
  Ix.toList . Ix.getEQ htmlRoute

lookupNotesByXmlRoute :: R 'R.Xml -> IxNote -> [Note]
lookupNotesByXmlRoute xmlRoute =
  Ix.toList . Ix.getEQ xmlRoute

lookupNotesByRoute :: (HasCallStack) => R.LMLRoute -> IxNote -> Maybe Note
lookupNotesByRoute r ix = do
  res <- nonEmpty $ Ix.toList $ Ix.getEQ r ix
  case res of
    note :| [] -> pure note
    _ -> error $ "ambiguous notes for route " <> show r

ancestorPlaceholderNote :: R.R 'Folder -> Note
ancestorPlaceholderNote r =
  let placeHolder =
        [ folderListingQuery
        , -- TODO: Ideally, we should use semantic tags, like <aside> (rather
          -- than <div>), to render these non-relevant content.
          B.Div (cls "emanote:placeholder-message")
            . one
            . B.Para
            $ [ B.Str
                  "Note: To override the auto-generated content here, create a file named one of: "
              , -- TODO: or, .org
                B.Span (cls "font-mono text-sm")
                  $ one
                  $ B.Str
                  $ oneOfLmlFilenames r
              ]
        ]
   in mkEmptyNoteWith (R.defaultLmlRoute r) placeHolder
  where
    folderListingQuery =
      B.CodeBlock (cls "query") "path:./*"

cls :: Text -> B.Attr
cls x =
  ("", one x, mempty) :: B.Attr

missingNote :: R.R ext -> Text -> Note
missingNote route404 urlPath =
  mkEmptyNoteWith (R.defaultLmlRoute route404)
    $ one
    $ B.Para
      [ B.Str "No note has the URL "
      , B.Code B.nullAttr $ "/" <> urlPath
      , -- TODO: org
        B.Span (cls "font-mono text-sm")
          $ one
          $ B.Str
          $ ". You may create a file with that name, ie. one of: "
          <> oneOfLmlFilenames route404
      ]

oneOfLmlFilenames :: R ext -> Text
oneOfLmlFilenames r =
  T.intercalate
    ", "
    (toText . R.withLmlRoute R.encodeRoute <$> R.possibleLmlRoutes r)

ambiguousNoteURL :: FilePath -> NonEmpty R.LMLRoute -> Note
ambiguousNoteURL urlPath rs =
  mkEmptyNoteWith (head rs)
    $ [ B.Para
          [ B.Str "The URL "
          , B.Code B.nullAttr $ toText urlPath
          , B.Str " is ambiguous, as more than one note (see list below) use it. To fix this, specify a different slug for these notes:"
          ]
      ]
    <> one candidates
  where
    candidates :: B.Block
    candidates =
      B.BulletList
        $ toList rs
        <&> \(R.lmlRouteCase -> r) ->
          [ B.Plain $ one $ B.Str "  "
          , B.Plain $ one $ B.Code B.nullAttr $ show r
          ]

mkEmptyNoteWith :: R.LMLRoute -> [B.Block] -> Note
mkEmptyNoteWith someR (Pandoc mempty -> doc) =
  mkNoteWith someR Nothing doc meta mempty
  where
    meta = Aeson.Null

mkNoteWith :: R.LMLRoute -> Maybe (Loc, FilePath) -> Pandoc -> Aeson.Value -> [Text] -> Note
mkNoteWith r src doc' meta errs =
  let (doc'', tit) = queryNoteTitle r doc' meta
      feed = queryNoteFeed meta
      doc = if null errs then doc'' else pandocPrepend (errorDiv errs) doc''
   in Note r src doc meta tit errs feed
  where
    -- Prepend to block to the beginning of a Pandoc document (never before H1)
    pandocPrepend :: B.Block -> Pandoc -> Pandoc
    pandocPrepend prefix (Pandoc docMeta blocks) =
      let blocks' = case blocks of
            (h1@(B.Header 1 _ _) : rest) ->
              h1 : prefix : rest
            _ -> prefix : blocks
       in Pandoc docMeta blocks'
    errorDiv :: [Text] -> B.Block
    errorDiv s =
      B.Div (cls "emanote:error") $ B.Para [B.Strong $ one $ B.Str "Emanote Errors 😔"] : (B.Para . one . B.Str <$> s)

parseNote ::
  forall m.
  (MonadIO m, MonadLogger m) =>
  ScriptingEngine ->
  [FilePath] ->
  R.LMLRoute ->
  (Loc, FilePath) ->
  Text ->
  m Note
parseNote scriptingEngine pluginBaseDir r src@(_, fp) s = do
  ((doc, meta), errs) <- runWriterT $ do
    case r of
      R.LMLRoute_Md _ ->
        parseNoteMarkdown scriptingEngine pluginBaseDir r fp s
      R.LMLRoute_Org _ -> do
        parseNoteOrg s
  let metaWithDateFromPath = case P.parse dateParser mempty (takeFileName fp) of
        Left _ -> meta
        Right date -> SData.modifyAeson (pure "date") (Just . fromMaybe (Aeson.String date)) meta
  pure $ mkNoteWith r (Just src) doc metaWithDateFromPath errs
  where
    dateParser = do
      year <- replicateM 4 P.digit
      _ <- P.char '-'
      month <- replicateM 2 P.digit
      _ <- P.char '-'
      day <- replicateM 2 P.digit
      _ <- P.satisfy (not . isDigit)
      pure $ toText $ mconcat [year, "-", month, "-", day]

parseNoteOrg :: (MonadWriter [Text] m) => Text -> m (Pandoc, Aeson.Value)
parseNoteOrg s =
  case runPure $ readOrg readerOpts s of
    Left err -> do
      tell [show err]
      pure (mempty, defaultFrontMatter)
    Right doc ->
      -- TODO: Merge Pandoc's Meta in here?
      pure (preparePandoc doc, defaultFrontMatter)
  where
    readerOpts = def {readerExtensions = extensionsFromList (exts)}
    exts = [Ext_auto_identifiers]

parseNoteMarkdown ::
  (MonadIO m, MonadLogger m) =>
  ScriptingEngine ->
  [FilePath] ->
  R.LMLRoute ->
  FilePath ->
  Text ->
  WriterT [Text] m (Pandoc, Aeson.Value)
parseNoteMarkdown scriptingEngine pluginBaseDir r fp md = do
  case Markdown.parseMarkdown fp md of
    Left err -> do
      tell [err]
      pure (mempty, defaultFrontMatter)
    Right (withAesonDefault defaultFrontMatter -> frontmatter, doc') -> do
      -- Apply the various transformation filters.
      --
      -- Some are user-defined; some builtin. They operate on Pandoc, or the
      -- frontmatter meta.
      filterPaths <- fmap catMaybes $ forM (SData.lookupAeson @[FilePath] mempty ("pandoc" :| ["filters"]) frontmatter) $ \p -> do
        res :: [FilePath] <- flip mapMaybeM pluginBaseDir $ \baseDir -> do
          doesPathExist (baseDir </> p) >>= \case
            False -> do
              pure Nothing
            True ->
              pure $ Just $ baseDir </> p
        case res of
          [] -> do
            tell [toText $ "Pandoc filter " <> p <> " not found in any of: " <> show pluginBaseDir]
            pure Nothing
          (x : _) -> pure $ Just x

      doc <- applyPandocFilters scriptingEngine filterPaths $ preparePandoc doc'
      let meta = applyNoteMetaFilters doc r frontmatter
      pure (doc, meta)
  where
    withAesonDefault default_ mv =
      fromMaybe default_ mv
        `SData.mergeAeson` default_

defaultFrontMatter :: Aeson.Value
defaultFrontMatter =
  Aeson.toJSON $ Map.fromList @Text @[Text] $ one ("tags", [])

applyNoteMetaFilters :: Pandoc -> R.LMLRoute -> Aeson.Value -> Aeson.Value
applyNoteMetaFilters doc r =
  addTagsFromMarkdown
    >>> addDescriptionFromBody
    >>> addImageFromBody
  where
    -- DESIGN: In retrospect, this is like a Pandoc lua filter?
    addTagsFromMarkdown frontmatter =
      frontmatter
        & AO.key "tags"
        % AO._Array
        .~ ( fromList
              . fmap Aeson.toJSON
              $ ordNub
              $ mconcat
                [ tagsFromFrontmatter frontmatter
                , -- Include inline tags from note body
                  tagsFromBody
                , -- Include tags for daily notes
                  tagsForDailyNote
                ]
           )
    tagsFromFrontmatter =
      SData.lookupAeson @[HT.Tag] mempty (one "tags")
    tagsFromBody = HT.inlineTagsInPandoc doc
    tagsForDailyNote = maybe mempty dayTags $ Calendar.parseRouteDay r
    dayTags day =
      let (y, m, d) = toGregorian day
          pad2 = toText @String . printf "%02d"
       in [HT.Tag $ "calendar/" <> show y <> "/" <> pad2 m <> "/" <> pad2 d]
    addDescriptionFromBody =
      overrideAesonText ("page" :| ["description"]) $ \case
        B.Para is -> [WL.plainify is]
        _ -> mempty
    -- FIXME this doesn't take splice rendering into account. Specifically,
    -- `![[foo.jpeg]]` is not handled at all.
    addImageFromBody =
      overrideAesonText ("page" :| ["image"]) $ \case
        B.Image _ _ (url, _) -> [url]
        _ -> mempty
    overrideAesonText :: forall a. (W.Walkable a Pandoc) => NonEmpty Text -> (a -> [Text]) -> Aeson.Value -> Aeson.Value
    overrideAesonText key f frontmatter =
      SData.mergeAesons
        $ frontmatter
        :| maybeToList
          ( do
              guard $ "" == SData.lookupAeson @Text "" key frontmatter
              val <- viaNonEmpty head $ W.query f doc
              pure $ SData.oneAesonText (toList key) val
          )

makeLenses ''Note
