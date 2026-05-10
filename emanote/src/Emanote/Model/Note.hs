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
import Emanote.Model.Note.Filter qualified as NoteFilter
import Emanote.Model.SData qualified as SData
import Emanote.Model.Title qualified as Tit
import Emanote.Pandoc.BuiltinFilters (preparePandoc)
import Emanote.Pandoc.Diagnostic qualified as Diagnostic
import Emanote.Pandoc.Markdown.Parser qualified as Markdown
import Emanote.Pandoc.Markdown.Syntax.HashTag qualified as HT
import Emanote.Route qualified as R
import Emanote.Route.Ext (FileType (Folder))
import Emanote.Route.ModelRoute qualified as MR
import Emanote.Route.R (R)
import Emanote.Source.Loc (Loc)
import Network.URI.Slug (Slug)
import Optics.Core ((%), (.~))
import Optics.TH (makeLenses)
import Relude
import System.FilePath (takeDirectory, takeFileName)
import Text.Pandoc (readerExtensions, runPure)
import Text.Pandoc.Builder qualified as B
import Text.Pandoc.Definition (Pandoc (..))
import Text.Pandoc.Extensions
import Text.Pandoc.Readers.Org (readOrg)
import Text.Pandoc.Scripting (ScriptingEngine)
import Text.Pandoc.Walk qualified as W
import Text.Parsec qualified as P
import Text.Printf (printf)

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
  , _notePandocFilterDeclarations :: NoteFilter.PandocFilterDeclarations
  , _noteTitle :: Tit.Title
  , _noteErrors :: [Text]
  , _noteFeed :: Maybe Feed
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Aeson.ToJSON)

newtype RAncestor = RAncestor {unRAncestor :: R 'R.Folder}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Aeson.ToJSON)

{- | Per-note IxSet indices. Notably absent: a tag index. Tags can be
inherited from sibling YAML cascade (see issue #352), and that
requires the model's SData — which 'ixFun' (a pure @Note -> [k]@)
can't see. Tag-based queries go through 'Emanote.Model.Meta.modelTags'
and the per-note effective-tag helper, which both consult cascade.
-}
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
      (ixFun $ maybeToList . noteSlug)

-- | All possible wiki-links that refer to this note.
noteSelfRefs :: Note -> NonEmpty WL.WikiLink
noteSelfRefs =
  routeSelfRefs
    . _noteRoute
  where
    routeSelfRefs :: R.LMLRoute -> NonEmpty WL.WikiLink
    routeSelfRefs =
      fromList
        . ordNub
        . toList
        . fmap snd
        . R.withLmlRoute (WL.allowedWikiLinks . R.unRoute)

noteAncestors :: Note -> [RAncestor]
noteAncestors =
  maybe [] (toList . fmap RAncestor . R.routeInits) . noteParent

noteParent :: Note -> Maybe (R 'R.Folder)
noteParent = R.withLmlRoute R.routeParent . _noteRoute

{- | Folder used as the base for resolving relative URLs from this note.

For most notes, this is the parent folder of the note's route. For notes
loaded from @\<dir\>/index.md@ (or @index.org@) the canonicalized route
already drops the @index@ slug — but relative URLs should still resolve
against the actual @\<dir\>/@ directory, not its parent. We use the note's
on-disk source path (when known) to derive the correct base.

See <https://github.com/srid/emanote/issues/608>.
-}
noteResolveLinkBase :: Note -> Maybe (R 'R.Folder)
noteResolveLinkBase note =
  case _noteSource note of
    Just (_, fp) -> resolveLinkBaseFromFilePath fp
    Nothing -> noteParent note

-- | Folder route corresponding to the directory containing the given file.
resolveLinkBaseFromFilePath :: FilePath -> Maybe (R 'R.Folder)
resolveLinkBaseFromFilePath fp =
  case takeDirectory fp of
    "." -> Nothing
    "" -> Nothing
    "/" -> Nothing
    dir -> R.mkRouteFromFilePath dir

hasChildNotes :: R 'Folder -> IxNote -> Bool
hasChildNotes r =
  not . Ix.null . Ix.getEQ (Just r)

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
  case noteSlug note of
    Just slugs ->
      -- An explicit `slug:` is taken at face value: the user typed the URL.
      R.mkRouteFromSlugs slugs
    Nothing ->
      -- File-path-derived: route through the canonical LML→HTML conversion
      -- so the trailing-index expansion isn't forgotten (see #542).
      MR.lmlToHtmlRoute _noteRoute

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
      , B.Str ". "
      , B.Span (cls "font-mono text-sm") $ one $ B.Str $ missingUrlHint urlPath route404
      ]

{- | Hint text to show on the missing-link page. The naive form
("create one of: foo.xml.md, foo.xml.org") is misleading whenever the
requested URL has a non-LML extension — `foo.xml` is far more likely
a static asset (or feed-enabled `foo.md`) than a note literally named
`foo.xml.md`. See #547.
-}
missingUrlHint :: Text -> R.R ext -> Text
missingUrlHint urlPath route404
  | Just stem <- T.stripSuffix ".xml" urlPath =
      "You may create a static file `"
        <> urlPath
        <> "`, or a feed-enabled note `"
        <> stem
        <> ".md`."
  | hasNonLmlExt urlPath =
      "You may create a static file with that name."
  | otherwise =
      "You may create a file with that name, ie. one of: "
        <> oneOfLmlFilenames route404

{- | True when @url@ has an extension that is neither LML (md/org)
nor the canonical HTML/XML cases handled elsewhere in the hint.
-}
hasNonLmlExt :: Text -> Bool
hasNonLmlExt url = case T.breakOnEnd "." url of
  (prefix, ext) -> not (T.null prefix) && ext `notElem` ["md", "org", "html", "xml"]

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
  mkNoteWith someR Nothing doc meta mempty mempty
  where
    meta = Aeson.Null

mkNoteWith :: R.LMLRoute -> Maybe (Loc, FilePath) -> Pandoc -> Aeson.Value -> NoteFilter.PandocFilterDeclarations -> [Text] -> Note
mkNoteWith r src doc' meta filterDeclarations errs =
  let (doc'', tit) = queryNoteTitle r doc' meta
      feed = queryNoteFeed meta
      doc = if null errs then doc'' else pandocPrepend (errorDiv "yaml" "Emanote Errors 😔" errs) doc''
   in Note
        { _noteRoute = r
        , _noteSource = src
        , _noteDoc = doc
        , _noteMeta = meta
        , _notePandocFilterDeclarations = filterDeclarations
        , _noteTitle = tit
        , _noteErrors = errs
        , _noteFeed = feed
        }
  where
    -- Prepend to block to the beginning of a Pandoc document (never before H1)
    pandocPrepend :: B.Block -> Pandoc -> Pandoc
    pandocPrepend prefix (Pandoc docMeta blocks) =
      let blocks' = case blocks of
            (h1@(B.Header 1 _ _) : rest) ->
              h1 : prefix : rest
            _ -> prefix : blocks
       in Pandoc docMeta blocks'

{- | Shared builder for diagnostic banners. The @category@ is what
'Diagnostic.errorBlock' turns into the @emanote:error:<category>@ variant
class, so callers stay honest about *why* a banner is being shown — yaml
cascade errors, markdown parse errors, and Lua filter errors each carry
their own CSS hook for theming and selectors.
-}
errorDiv :: Text -> Text -> [Text] -> B.Block
errorDiv category header errs =
  Diagnostic.errorBlock category
    $ B.Para [B.Strong $ one $ B.Str header]
    : (B.Para . one . B.Str <$> errs)

{- | Result of parsing a single note's source: the parsed 'Note' plus
side-channel information the patcher needs to keep its indices in
sync. The dep lists carry filter paths *as written* in the note-local
Lua filter declarations, split by phase so the dependency index can
record each kind of edge — see "Emanote.Model.SourceDependencies".
The @parse@ list reflects only filters that actually ran (IO-rejected
parse-time filters are dropped); the @render@ list passes through
verbatim because render-time filters are resolved at render time.
-}
data ParseResult = ParseResult
  { parsedNote :: Note
  , luaParseFilterDeps :: [FilePath]
  , luaRenderFilterDeps :: [FilePath]
  }

parseNote ::
  forall m.
  (MonadIO m, MonadLogger m) =>
  ScriptingEngine ->
  [FilePath] ->
  R.LMLRoute ->
  (Loc, FilePath) ->
  Text ->
  m ParseResult
parseNote scriptingEngine pluginBaseDir r src@(_, fp) s = do
  ((doc, meta, filterDeclarations, parseFilters, renderFilters), errs) <- runWriterT $ do
    case r of
      R.LMLRoute_Md _ ->
        parseNoteMarkdown scriptingEngine pluginBaseDir r fp s
      R.LMLRoute_Org _ ->
        parseNoteOrg scriptingEngine pluginBaseDir s
  let metaWithDateFromPath = case P.parse dateParser mempty (takeFileName fp) of
        Left _ -> meta
        Right date -> SData.modifyAeson (pure "date") (Just . fromMaybe (Aeson.String date)) meta
  pure
    ParseResult
      { parsedNote = mkNoteWith r (Just src) doc metaWithDateFromPath filterDeclarations errs
      , luaParseFilterDeps = parseFilters
      , luaRenderFilterDeps = renderFilters
      }
  where
    dateParser = do
      year <- replicateM 4 P.digit
      _ <- P.char '-'
      month <- replicateM 2 P.digit
      _ <- P.char '-'
      day <- replicateM 2 P.digit
      _ <- P.satisfy (not . isDigit)
      pure $ toText $ mconcat [year, "-", month, "-", day]

parseNoteOrg ::
  (MonadIO m, MonadLogger m) =>
  ScriptingEngine ->
  [FilePath] ->
  Text ->
  WriterT [Text] m (Pandoc, Aeson.Value, NoteFilter.PandocFilterDeclarations, [FilePath], [FilePath])
parseNoteOrg scriptingEngine pluginBaseDir s = do
  (doc', meta) <- parseNoteOrgDocument s
  let directives = parseOrgDirectives s
      filterDeclarations = mempty {NoteFilter.pfdParseFilters = odPandocFilters directives}
  forM_ (odUnsupportedDirectives directives) $ \(key, _) ->
    tell ["Org keyword " <> key <> " is not supported. Render-time Lua filters can only be declared from Markdown frontmatter (pandoc.filters.render.html); see lua-filters guide."]
  (doc, filterDeps) <- NoteFilter.applyParsePandocFilters scriptingEngine pluginBaseDir filterDeclarations doc'
  pure (doc, meta, filterDeclarations, filterDeps, [])

parseNoteOrgDocument :: (MonadWriter [Text] m) => Text -> m (Pandoc, Aeson.Value)
parseNoteOrgDocument s =
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

data OrgDirectives = OrgDirectives
  { odPandocFilters :: [FilePath]
  -- ^ Parse-time Pandoc Lua filters declared via @#+PANDOC_FILTERS:@.
  , odUnsupportedDirectives :: [(Text, Text)]
  -- ^ Pandoc-filter-shaped keywords Org can't act on yet (render-time
  -- variants); surfaced so 'parseNoteOrg' can warn the user instead of
  -- silently dropping the declaration.
  }
  deriving stock (Eq, Show)

parseOrgDirectives :: Text -> OrgDirectives
parseOrgDirectives s =
  OrgDirectives
    { odPandocFilters = toString . snd <$> parseTimeKws
    , odUnsupportedDirectives = unsupportedKws
    }
  where
    headerKeywords =
      mapMaybe parseOrgKeyword
        $ takeWhile orgHeaderLine
        $ dropWhile (T.null . T.strip)
        $ lines s
    parseTimeKws = filter (\(k, _) -> k `elem` parseTimeKeys) headerKeywords
    unsupportedKws =
      filter (\(k, _) -> k `notElem` parseTimeKeys && any (`T.isPrefixOf` k) unsupportedKeyPrefixes) headerKeywords
    parseTimeKeys = ["#+pandoc_filters", "#+pandoc.filters"]
    unsupportedKeyPrefixes = ["#+pandoc_filters_render", "#+pandoc.filters.render"]
    orgHeaderLine line =
      let stripped = T.strip line
       in T.null stripped || "#+" `T.isPrefixOf` stripped
    parseOrgKeyword line = do
      let (rawKey, rawValue) = T.breakOn ":" $ T.stripStart line
      guard $ not $ T.null rawValue
      let value = T.strip $ T.drop 1 rawValue
      guard $ not $ T.null value
      pure (T.toCaseFold rawKey, value)

parseNoteMarkdown ::
  (MonadIO m, MonadLogger m) =>
  ScriptingEngine ->
  [FilePath] ->
  R.LMLRoute ->
  FilePath ->
  Text ->
  WriterT [Text] m (Pandoc, Aeson.Value, NoteFilter.PandocFilterDeclarations, [FilePath], [FilePath])
parseNoteMarkdown scriptingEngine pluginBaseDir r fp md = do
  case Markdown.parseMarkdown fp md of
    Left err -> do
      tell [err]
      pure (mempty, defaultFrontMatter, mempty, [], [])
    Right (withAesonDefault defaultFrontMatter -> frontmatter, doc') -> do
      tell $ NoteFilter.filterDeclarationShapeErrors frontmatter
      let filterDeclarations = NoteFilter.lookupPandocFilterDeclarations frontmatter
      (doc, parseDeps) <- NoteFilter.applyParsePandocFilters scriptingEngine pluginBaseDir filterDeclarations $ preparePandoc doc'
      let meta = applyNoteMetaFilters doc r frontmatter
      pure (doc, meta, filterDeclarations, parseDeps, NoteFilter.pfdRenderHtmlFilters filterDeclarations)
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
      let (y, m, _d) = toGregorian day
          pad2 = toText @String . printf "%02d"
       in [HT.Tag $ "calendar/" <> show y <> "/" <> pad2 m]
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
