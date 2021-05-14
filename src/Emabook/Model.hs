{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Emabook.Model where

import Control.Monad.Writer.Strict (MonadWriter (tell))
import Data.Data (Data)
import Data.Default (Default (..))
import Data.IxSet.Typed (Indexable (..), IxSet, ixFun, ixGen, ixList, (@+), (@=))
import qualified Data.IxSet.Typed as Ix
import qualified Data.Text as T
import Data.Tree (Tree)
import qualified Data.YAML as Y
import Ema (Ema (..), Slug)
import qualified Ema
import qualified Ema.Helper.PathTree as PathTree
import qualified Emabook.PandocUtil as PandocUtil
import Emabook.Route (MarkdownRoute)
import qualified Emabook.Route as R
import qualified Emabook.Template as T
import Text.Pandoc.Definition (Pandoc (..))
import qualified Text.Pandoc.Definition as B
import qualified Text.Pandoc.Walk as W

-- | This is our Ema "model" -- the app state used to generate our site.
--
-- It contains the list of all markdown files, parsed as Pandoc AST.
data Model = Model
  { modelNotes :: IxNote,
    modelNav :: [Tree Slug],
    modelHeistTemplate :: T.TemplateState
  }

instance Default Model where
  def = Model Ix.empty mempty (Left $ one "Heist state not yet loaded")

data Note = Note
  { noteDoc :: Pandoc,
    noteMeta :: Meta,
    noteRoute :: MarkdownRoute
  }
  deriving (Eq, Ord, Data, Show)

-- | Set of WikiLinks that refer to a note.
newtype SelfRef = SelfRef {unSelfRef :: R.WikiLinkTarget}
  deriving (Eq, Ord, Data, Show)

-- | Outgoing links from a note; can be [[Foo]] or Foo/bar.md
newtype OutgoingRef = OutgoingRef {unOutgoingRef :: Either R.WikiLinkTarget R.MarkdownRoute}
  deriving (Eq, Ord, Data, Show)

-- | Wiki-links that refer to this note.
noteSelfRefs :: Note -> [SelfRef]
noteSelfRefs = fmap SelfRef . toList . R.allowedWikiLinkTargets . noteRoute

outgoingRefs :: Note -> [OutgoingRef]
outgoingRefs =
  fmap OutgoingRef . extractLinks . noteDoc
  where
    extractLinks :: Pandoc -> [Either R.WikiLinkTarget MarkdownRoute]
    extractLinks =
      W.query $ \i -> maybeToList $ do
        B.Link _ _ (url, _) <- pure i
        parseUrl url

noteTitle :: Note -> Text
noteTitle Note {..} =
  fromMaybe (R.markdownRouteFileBase noteRoute) $ PandocUtil.getPandocTitle noteDoc

type NoteIxs = '[MarkdownRoute, SelfRef, OutgoingRef]

type IxNote = IxSet NoteIxs Note

instance Indexable NoteIxs Note where
  indices =
    ixList
      (ixGen (Proxy :: Proxy MarkdownRoute))
      (ixFun noteSelfRefs)
      (ixFun outgoingRefs)

data Meta = Meta
  { -- | Indicates the order of the Markdown file in sidebar tree, relative to
    -- its siblings. Default value: 0.
    order :: Int,
    tags :: [Text]
  }
  deriving (Eq, Show, Ord, Data)

instance Y.FromYAML Meta where
  parseYAML = Y.withMap "FrontMatter" $ \m ->
    Meta
      <$> (fromMaybe def <$> m Y..:? "order")
      <*> (fromMaybe mempty <$> m Y..:? "tags")

instance Default Meta where
  def = Meta def mempty

modelLookup :: MarkdownRoute -> Model -> Maybe Note
modelLookup k =
  Ix.getOne . Ix.getEQ k . modelNotes

modelLookupMeta :: MarkdownRoute -> Model -> Meta
modelLookupMeta k =
  maybe def noteMeta . modelLookup k

modelLookupRouteByWikiLink :: R.WikiLinkTarget -> Model -> [MarkdownRoute]
modelLookupRouteByWikiLink wl model =
  fmap noteRoute . Ix.toList $ modelNotes model @= SelfRef wl

modelLookupBacklinks :: MarkdownRoute -> Model -> [Note]
modelLookupBacklinks r model =
  let refsToSelf =
        (OutgoingRef . Left <$> toList (R.allowedWikiLinkTargets r))
          <> [OutgoingRef $ Right r]
   in Ix.toList $ modelNotes model @+ refsToSelf

modelLookupTitle :: MarkdownRoute -> Model -> Text
modelLookupTitle r =
  maybe (R.markdownRouteFileBase r) noteTitle . modelLookup r

modelInsert :: MarkdownRoute -> (Meta, Pandoc) -> Model -> Model
modelInsert k v model =
  let modelNotes' = Ix.updateIx k note (modelNotes model)
      note = Note (snd v) (fst v) k
   in model
        { modelNotes = modelNotes',
          modelNav =
            PathTree.treeInsertPathMaintainingOrder
              (sortKey modelNotes' . R.MarkdownRoute)
              (R.unMarkdownRoute k)
              (modelNav model)
        }
  where
    -- Sort by `order` meta, falling back to title.
    sortKey notes r = fromMaybe (def, R.markdownRouteFileBase r) $ do
      note <- Ix.getOne $ Ix.getEQ r notes
      pure $
        (,)
          (order $ noteMeta note)
          (noteTitle note)

modelDelete :: MarkdownRoute -> Model -> Model
modelDelete k model =
  model
    { modelNotes = Ix.deleteIx k (modelNotes model),
      modelNav = PathTree.treeDeletePath (R.unMarkdownRoute k) (modelNav model)
    }

modelSetHeistTemplate :: T.TemplateState -> Model -> Model
modelSetHeistTemplate v model =
  model {modelHeistTemplate = v}

instance Ema Model MarkdownRoute where
  -- Convert a route to URL slugs
  encodeRoute = \case
    R.MarkdownRoute ("index" :| []) -> mempty
    R.MarkdownRoute paths -> toList paths

  -- Parse our route from URL slugs
  --
  -- For eg., /foo/bar maps to slugs ["foo", "bar"], which in our app gets
  -- parsed as representing the route to /foo/bar.md.
  decodeRoute = \case
    (nonEmpty -> Nothing) ->
      pure $ R.MarkdownRoute $ one "index"
    (nonEmpty -> Just slugs) -> do
      -- Heuristic to let requests to static files (eg: favicon.ico) to pass through
      guard $ not (any (T.isInfixOf "." . Ema.unSlug) slugs)
      pure $ R.MarkdownRoute slugs

  -- Which routes to generate when generating the static HTML for this site.
  staticRoutes (fmap noteRoute . Ix.toList . modelNotes -> mdRoutes) =
    mdRoutes

  -- All static assets (relative to input directory) go here.
  -- Not all of these may exist.
  staticAssets _ =
    ["favicon.jpeg", "favicon.svg", "static"]

-- | Accumulate broken links in Writer.
sanitizeMarkdown ::
  forall m w.
  (MonadWriter w m, w ~ [(MarkdownRoute, Text)]) =>
  Model ->
  MarkdownRoute ->
  Pandoc ->
  m Pandoc
sanitizeMarkdown model docRoute doc =
  doc
    -- Eliminate H1, because we are handling it separately.
    & PandocUtil.withoutH1
    & fmap rewriteMdLinks . rewriteWikiLinks
  where
    -- Resolve [[Bar/Foo]], etc.
    rewriteWikiLinks = do
      PandocUtil.rewriteRelativeLinksM $ \url ->
        fmap (fromMaybe url) . runMaybeT @m $ do
          guard $ not $ isStaticAssetUrl url
          Left wl <- hoistMaybe $ parseUrl url
          case nonEmpty (modelLookupRouteByWikiLink wl model) of
            Nothing -> do
              tell $ one (docRoute, url)
              -- TODO: Set an attribute for broken links, so templates can style it accordingly
              -- TODO: Return url as is, so backlinks work?
              pure "/EmaNotFound"
            Just targets ->
              -- TODO: Deal with ambiguous targets here
              pure $ Ema.routeUrl $ head targets
    -- Resolve Bar/Foo.md
    rewriteMdLinks =
      PandocUtil.rewriteRelativeLinks $ \url -> fromMaybe url $ do
        Right r <- parseUrl url
        pure $ Ema.routeUrl r
    isStaticAssetUrl url =
      any (\asset -> ("/" <> toText asset) `T.isPrefixOf` url) (staticAssets $ Proxy @MarkdownRoute)

-- | Parse a URL string
parseUrl :: Text -> Maybe (Either R.WikiLinkTarget MarkdownRoute)
parseUrl url = do
  guard $ not $ "://" `T.isInfixOf` url
  fmap Left (R.mkWikiLinkTargetFromUrl url)
    <|> fmap Right (R.mkMarkdownRouteFromFilePath $ toString url)