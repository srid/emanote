{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Emanote.Model.Note where

import Commonmark.Extensions.WikiLink qualified as WL
import Control.DeepSeq (NFData)
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
import Heist.Extra.Splices.Pandoc.TaskList qualified as TaskList
import Network.URI.Slug (Slug)
import Optics.Core ((%), (.~))
import Optics.TH (makeLenses)
import Relude
import System.FilePath (takeDirectory, takeFileName)
import Text.Pandoc (readerExtensions, runPure)
import Text.Pandoc.Builder qualified as B
import Text.Pandoc.Definition (Pandoc (..))
import Text.Pandoc.Extensions
import Text.Pandoc.LinkContext qualified as LC
import Text.Pandoc.Readers.Org (readOrg)
import Text.Pandoc.Scripting (ScriptingEngine)
import Text.Pandoc.Walk qualified as W
import Text.Parsec qualified as P
import Text.Printf (printf)
import Text.Show qualified as Show

data Feed = Feed
  { _feedEnable :: Bool
  , _feedTitle :: Maybe Text
  , _feedLimit :: Maybe Word
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON, NFData)

{- | A Note keeps the raw markdown/org source text plus a handful of
small, pre-extracted facts about it (title, frontmatter, outgoing link
URLs, task-list items, …). '_noteDoc' is intentionally a *lazy* field
populated with a closure that re-parses '_noteSourceText' on first
access; Haskell's normal lazy-evaluation semantics then memoise the
parsed Pandoc back into the field, so subsequent accesses (and embeds)
pay no extra parse cost. The driving cost on a 4500-file notebook is
that the in-memory Pandoc is ~16× the source bytes (#66); deferring the
parse means notes that are never rendered never materialise their
Pandoc, and the live memory floor drops to roughly the source size.

For three categories the parse cannot be deferred and 'mkNoteWith' binds
'_noteDoc' to an already-evaluated Pandoc:

* notes with parse-time Lua filters — the filter pass runs in IO and
  mutates the AST, so a pure re-parse would silently skip it;
* synthetic notes constructed via 'mkEmptyNoteWith' (folder placeholders,
  diagnostic banners) — they have no on-disk source to re-parse from;
* notes that carried parse errors — 'mkNoteWith' prepends an error
  banner Block which is not visible to the re-parse path.
-}
data NoteLink = NoteLink
  { _nlUrl :: !Text
  , _nlAttrs :: ![(Text, Text)]
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Aeson.ToJSON, NFData)

data NoteTask = NoteTask
  { _ntChecked :: !Bool
  , _ntDescription :: ![B.Inline]
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Aeson.ToJSON, NFData)

data Note = Note
  { _noteRoute :: R.LMLRoute
  , _noteSource :: Maybe (Loc, FilePath)
  -- ^ The layer from which this note came. Nothing if the note was auto-generated.
  , _noteSourceText :: !Text
  -- ^ Raw source bytes (markdown or org). Empty for synthetic notes.
  , _noteMeta :: Aeson.Value
  , _notePandocFilterDeclarations :: NoteFilter.PandocFilterDeclarations
  , _noteTitle :: Tit.Title
  , _noteErrors :: [Text]
  , _noteFeed :: Maybe Feed
  , _noteLinks :: ![NoteLink]
  -- ^ URLs (with their HTML attributes) of every link found in the
  -- parsed Pandoc, extracted once at parse time so 'noteRels' does not
  -- need to re-walk the AST.
  , _noteTaskList :: ![NoteTask]
  -- ^ Task-list items found in the parsed Pandoc, extracted once at
  -- parse time so the task index does not need the full AST.
  , _noteDoc :: Pandoc
  -- ^ The post-filter Pandoc. /Lazy on purpose/: the per-note value is
  -- a closure over '_noteSourceText' that re-parses on first access and
  -- memoises by Haskell's normal evaluation semantics. Until a renderer
  -- forces this field, the Pandoc AST is not in memory — that is the
  -- whole point of the (#66) optimisation. Notes that cannot be
  -- re-parsed purely (Lua filters, synthetic notes, parse errors) bind
  -- this field directly to the already-evaluated Pandoc so the first
  -- access is free.
  }
  deriving stock (Generic)
  deriving anyclass (Aeson.ToJSON)

-- 'Eq' and 'Ord' for 'Note' must not force '_noteDoc'. IxSet stores
-- 'Note' under an 'Ord' index, and a derived 'Ord' would compare the
-- lazy Pandoc field — re-parsing on every insertion-time comparison
-- defeats the deferred-parse design. Compare by route only; routes are
-- already unique within the IxSet via @ixFun (one . _noteRoute)@.
instance Eq Note where
  a == b = _noteRoute a == _noteRoute b

instance Ord Note where
  compare a b = compare (_noteRoute a) (_noteRoute b)

instance Show.Show Note where
  show n = "Note { _noteRoute = " <> show (_noteRoute n) <> ", … }"

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
  -- Synthetic notes have no on-disk source — pass an empty source text
  -- and force 'mkNoteWith' to retain the Pandoc by reporting "needs
  -- retention".
  mkNoteWith someR Nothing mempty doc meta mempty mempty
  where
    meta = Aeson.Null

mkNoteWith :: R.LMLRoute -> Maybe (Loc, FilePath) -> Text -> Pandoc -> Aeson.Value -> NoteFilter.PandocFilterDeclarations -> [Text] -> Note
mkNoteWith r src sourceText doc' meta filterDeclarations errs =
  let (doc'', tit) = queryNoteTitle r doc' meta
      feed = queryNoteFeed meta
      doc = if null errs then doc'' else pandocPrepend (errorDiv "yaml" "Emanote Errors 😔" errs) doc''
      links = extractNoteLinks doc
      tasks = extractNoteTasks doc
      -- The deferred-parse path is only safe when the post-filter
      -- Pandoc can be deterministically reproduced by re-parsing the
      -- source text. That excludes:
      --   * Lua-filter notes (filter pass runs in IO and mutates the AST),
      --   * synthetic notes (no on-disk source to re-parse),
      --   * notes that carried parse errors (an error banner Block has
      --     been prepended onto 'doc' and is not visible to re-parse).
      mustRetain =
        needsLuaFilter filterDeclarations
          || isNothing src
          || not (null errs)
      baseNote =
        Note
          { _noteRoute = r
          , _noteSource = src
          , _noteSourceText = sourceText
          , _noteMeta = meta
          , _notePandocFilterDeclarations = filterDeclarations
          , _noteTitle = tit
          , _noteErrors = errs
          , _noteFeed = feed
          , _noteLinks = links
          , _noteTaskList = tasks
          , _noteDoc = error "mkNoteWith: _noteDoc not bound"
          }
   in -- Branch *eagerly* on 'mustRetain' so each Note's '_noteDoc' field
      -- references only one of {doc, deferredDoc}, never both. With an
      -- @if mustRetain then doc else deferredDoc@ thunk the field would
      -- close over both branches and the original 'doc' could never be
      -- collected even on the deferred path — defeating (#66).
      if mustRetain
        then baseNote {_noteDoc = doc}
        else
          let deferredDoc = case r of
                R.LMLRoute_Md _ ->
                  let fp = maybe "" snd src
                   in reparseMd r fp sourceText meta
                R.LMLRoute_Org _ ->
                  reparseOrg sourceText
           in baseNote {_noteDoc = deferredDoc}
  where
    -- Prepend to block to the beginning of a Pandoc document (never before H1)
    pandocPrepend :: B.Block -> Pandoc -> Pandoc
    pandocPrepend prefix (Pandoc docMeta blocks) =
      let blocks' = case blocks of
            (h1@(B.Header 1 _ _) : rest) ->
              h1 : prefix : rest
            _ -> prefix : blocks
       in Pandoc docMeta blocks'

needsLuaFilter :: NoteFilter.PandocFilterDeclarations -> Bool
needsLuaFilter d =
  not (null (NoteFilter.pfdParseFilters d) && null (NoteFilter.pfdRenderHtmlFilters d))

{- | Pure markdown re-parse, structurally identical to the no-Lua-filter
path of 'parseNoteMarkdown'. Errors are silently swallowed because the
error-carrying notes opt out of deferred parsing in 'mkNoteWith' and
keep their already-evaluated Pandoc inline.
-}
reparseMd :: R.LMLRoute -> FilePath -> Text -> Aeson.Value -> Pandoc
reparseMd r fp md meta =
  case Markdown.parseMarkdown fp md of
    Left _ -> mempty
    Right (_frontmatter, doc') ->
      let doc = preparePandoc doc'
          -- Strip the H1 if the note's title came from it, matching the
          -- shape of the originally-stored '_noteDoc' (see 'mkNoteWith').
          (doc'', _tit) = queryNoteTitle r doc meta
       in doc''

reparseOrg :: Text -> Pandoc
reparseOrg s =
  case runPure $ readOrg readerOpts s of
    Left _ -> mempty
    Right doc -> preparePandoc doc
  where
    readerOpts = def {readerExtensions = extensionsFromList [Ext_auto_identifiers]}

extractNoteLinks :: Pandoc -> [NoteLink]
extractNoteLinks doc = do
  (url, instances) <- Map.toList $ LC.queryLinksWithContext doc
  (attrs, _ctx) <- reverse (toList instances)
  pure (NoteLink url attrs)

extractNoteTasks :: Pandoc -> [NoteTask]
extractNoteTasks doc =
  TaskList.queryTasks doc
    <&> \(checked, inlines) -> NoteTask checked inlines

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

data ParsedNoteSource = ParsedNoteSource
  { pnsDoc :: Pandoc
  , pnsMeta :: Aeson.Value
  , pnsFilterDeclarations :: NoteFilter.PandocFilterDeclarations
  }

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
  (ParsedNoteSource {..}, errs) <- runWriterT $ do
    case r of
      R.LMLRoute_Md _ ->
        parseNoteMarkdown scriptingEngine pluginBaseDir r fp s
      R.LMLRoute_Org _ ->
        parseNoteOrg scriptingEngine pluginBaseDir s
  let metaWithDateFromPath = case P.parse dateParser mempty (takeFileName fp) of
        Left _ -> pnsMeta
        Right date -> SData.modifyAeson (pure "date") (Just . fromMaybe (Aeson.String date)) pnsMeta
  pure $ mkNoteWith r (Just src) s pnsDoc metaWithDateFromPath pnsFilterDeclarations errs
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
  WriterT [Text] m ParsedNoteSource
parseNoteOrg scriptingEngine pluginBaseDir s = do
  (doc', meta) <- parseNoteOrgDocument s
  let filterDeclarations = NoteFilter.lookupOrgPandocFilterDeclarations s
  doc <- NoteFilter.applyParsePandocFilters scriptingEngine pluginBaseDir filterDeclarations doc'
  pure $ ParsedNoteSource doc meta filterDeclarations

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

parseNoteMarkdown ::
  (MonadIO m, MonadLogger m) =>
  ScriptingEngine ->
  [FilePath] ->
  R.LMLRoute ->
  FilePath ->
  Text ->
  WriterT [Text] m ParsedNoteSource
parseNoteMarkdown scriptingEngine pluginBaseDir r fp md = do
  case Markdown.parseMarkdown fp md of
    Left err -> do
      tell [err]
      pure $ ParsedNoteSource mempty defaultFrontMatter mempty
    Right (withAesonDefault defaultFrontMatter -> frontmatter, doc') -> do
      tell $ NoteFilter.filterDeclarationShapeErrors frontmatter
      let filterDeclarations = NoteFilter.lookupPandocFilterDeclarations frontmatter
      doc <- NoteFilter.applyParsePandocFilters scriptingEngine pluginBaseDir filterDeclarations $ preparePandoc doc'
      let meta = applyNoteMetaFilters doc r frontmatter
      pure $ ParsedNoteSource doc meta filterDeclarations
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
makeLenses ''NoteLink
makeLenses ''NoteTask
