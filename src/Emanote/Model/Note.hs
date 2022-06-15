{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Emanote.Model.Note where

import Control.Monad.Logger (MonadLogger)
import Control.Monad.Writer (MonadWriter (tell), WriterT, runWriterT)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Optics qualified as AO
import Data.Default
import Data.IxSet.Typed (Indexable (..), IxSet, ixFun, ixList)
import Data.IxSet.Typed qualified as Ix
import Data.Map.Strict qualified as Map
import Emanote.Model.Note.Filter (applyPandocFilters)
import Emanote.Model.SData qualified as SData
import Emanote.Model.Title qualified as Tit
import Emanote.Pandoc.Markdown.Parser qualified as Markdown
import Emanote.Pandoc.Markdown.Syntax.HashTag qualified as HT
import Emanote.Pandoc.Markdown.Syntax.WikiLink (plainify)
import Emanote.Pandoc.Markdown.Syntax.WikiLink qualified as WL
import Emanote.Route (FileType (Folder), R)
import Emanote.Route qualified as R
import Network.URI.Slug (Slug)
import Optics.Core ((%), (.~))
import Optics.TH (makeLenses)
import Relude
import System.FilePath ((</>))
import Text.Pandoc (runPure)
import Text.Pandoc.Builder qualified as B
import Text.Pandoc.Definition (Pandoc (..))
import Text.Pandoc.Readers.Org (readOrg)
import Text.Pandoc.Walk qualified as W

data Note = Note
  { _noteRoute :: R.LMLRoute,
    _noteDoc :: Pandoc,
    _noteMeta :: Aeson.Value,
    _noteTitle :: Tit.Title,
    _noteErrors :: [Text]
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
        . R.withLmlRoute WL.allowedWikiLinks

noteAncestors :: Note -> [RAncestor]
noteAncestors =
  maybe [] (toList . fmap RAncestor . R.routeInits) . noteParent

noteParent :: Note -> Maybe (R 'R.Folder)
noteParent = R.withLmlRoute R.routeParent . _noteRoute

hasChildNotes :: R 'Folder -> IxNote -> Bool
hasChildNotes r =
  not . Ix.null . Ix.getEQ r

noteTags :: Note -> [HT.Tag]
noteTags =
  fmap HT.Tag . maybeToMonoid . lookupMeta (one "tags")

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
      R.withLmlRoute coerce _noteRoute
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
              B.Span (cls "font-mono text-sm") $ one $ B.Str $ toText (R.withLmlRoute R.encodeRoute r)
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
  mkNoteWith someR doc meta mempty
  where
    meta = Aeson.Null

mkNoteWith :: R.LMLRoute -> Pandoc -> Aeson.Value -> [Text] -> Note
mkNoteWith r doc' meta errs =
  let doc = if null errs then doc' else pandocPrepend (errorDiv errs) doc'
      tit = queryNoteTitle r doc' meta
   in Note r doc meta tit errs
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
      where
        cls x = ("", one x, mempty) :: B.Attr

parseNote ::
  forall m.
  (MonadIO m, MonadLogger m) =>
  FilePath ->
  R.LMLRoute ->
  FilePath ->
  Text ->
  m Note
parseNote pluginBaseDir r fp s = do
  ((doc, meta), errs) <- runWriterT $ do
    case r of
      R.LMLRoute_Md _ ->
        parseNoteMarkdown pluginBaseDir fp s
      R.LMLRoute_Org _ -> do
        parseNoteOrg s
  pure $ mkNoteWith r doc meta errs

parseNoteOrg :: (MonadWriter [Text] m) => Text -> m (Pandoc, Aeson.Value)
parseNoteOrg s =
  case runPure $ readOrg def s of
    Left err -> do
      tell [show err]
      pure (mempty, defaultFrontMatter)
    Right doc ->
      pure (doc, defaultFrontMatter)

parseNoteMarkdown :: (MonadIO m, MonadLogger m) => FilePath -> FilePath -> Text -> WriterT [Text] m (Pandoc, Aeson.Value)
parseNoteMarkdown pluginBaseDir fp md = do
  case Markdown.parseMarkdown fp md of
    Left err -> do
      tell [err]
      pure (mempty, defaultFrontMatter)
    Right (withAesonDefault defaultFrontMatter -> frontmatter, doc') -> do
      -- Apply the various transformation filters.
      --
      -- Some are user-defined; some builtin. They operate on Pandoc, or the
      -- frontmatter meta.
      let filterPaths = (pluginBaseDir </>) <$> lookupAeson @[FilePath] mempty ("pandoc" :| ["filters"]) frontmatter
      doc <- applyPandocFilters filterPaths doc'
      let meta = applyNoteMetaFilters doc frontmatter
      pure (doc, meta)
  where
    withAesonDefault default_ mv =
      fromMaybe default_ mv
        `SData.mergeAeson` default_

defaultFrontMatter :: Aeson.Value
defaultFrontMatter =
  Aeson.toJSON $ Map.fromList @Text @[Text] $ one ("tags", [])

applyNoteMetaFilters :: Pandoc -> Aeson.Value -> Aeson.Value
applyNoteMetaFilters doc =
  addTagsFromBody
    >>> addDescriptionFromBody
    >>> addImageFromBody
  where
    -- Merge frontmatter tags with inline tags in Pandoc document.
    -- DESIGN: In retrospect, this is like a Pandoc lua filter?
    addTagsFromBody frontmatter =
      frontmatter
        & AO.key "tags" % AO._Array
        .~ ( fromList . fmap Aeson.toJSON $
               ordNub $
                 lookupAeson @[HT.Tag] mempty (one "tags") frontmatter
                   <> HT.inlineTagsInPandoc doc
           )
    addDescriptionFromBody =
      overrideAesonText ("page" :| ["description"]) $ \case
        B.Para is -> [plainify is]
        _ -> mempty
    -- FIXME this doesn't take splice rendering into account. Specifically,
    -- `![[foo.jpeg]]` is not handled at all.
    addImageFromBody =
      overrideAesonText ("page" :| ["image"]) $ \case
        B.Image _ _ (url, _) -> [url]
        _ -> mempty
    overrideAesonText :: forall a. (W.Walkable a Pandoc) => NonEmpty Text -> (a -> [Text]) -> Aeson.Value -> Aeson.Value
    overrideAesonText key f frontmatter =
      SData.mergeAesons $
        frontmatter
          :| maybeToList
            ( do
                guard $ "" == lookupAeson @Text "" key frontmatter
                val <- viaNonEmpty head $ W.query f doc
                pure $ oneAesonText (toList key) val
            )

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
