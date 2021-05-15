{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Emabook.Model where

import Control.Monad.Writer.Strict (MonadWriter (tell))
import qualified Data.Aeson as Aeson
import Data.Data (Data)
import Data.Default (Default (..))
import Data.IxSet.Typed (Indexable (..), IxSet, ixFun, ixGen, ixList, (@+), (@=))
import qualified Data.IxSet.Typed as Ix
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Tree (Tree)
import qualified Data.YAML as Y
import Data.YAML.ToJSON ()
import Ema (Ema (..), Slug)
import qualified Ema
import qualified Ema.Helper.PathTree as PathTree
import qualified Emabook.PandocUtil as PandocUtil
import Emabook.Route (MarkdownRoute)
import qualified Emabook.Route as R
import qualified Emabook.Template as T
import Text.Pandoc.Definition (Pandoc (..))
import qualified Text.Pandoc.Definition as B
import qualified Text.Pandoc.LinkContext as LC

-- | This is our Ema "model" -- the app state used to generate our site.
--
-- It contains the list of all markdown files, parsed as Pandoc AST.
data Model = Model
  { modelNotes :: IxNote,
    modelRels :: IxRel,
    modelNav :: [Tree Slug],
    modelSettings :: Aeson.Value,
    modelHeistTemplate :: T.TemplateState
  }

instance Default Model where
  def = Model Ix.empty Ix.empty mempty Aeson.Null (Left $ one "Heist state not yet loaded")

parseYaml :: Y.FromYAML a => FilePath -> Text -> Either Text a
parseYaml n (encodeUtf8 -> v) = do
  let mkError (loc, emsg) =
        toText $ n <> ":" <> Y.prettyPosWithSource loc v " error" <> emsg
  first mkError $ Y.decode1 v

data Note = Note
  { noteDoc :: Pandoc,
    noteMeta :: Meta,
    noteRoute :: MarkdownRoute
  }
  deriving (Eq, Ord, Data, Show)

-- | Set of WikiLinks that refer to a note.
newtype SelfRef = SelfRef {unSelfRef :: R.WikiLinkTarget}
  deriving (Eq, Ord, Data, Show)

-- | Wiki-links that refer to this note.
noteSelfRefs :: Note -> [SelfRef]
noteSelfRefs = fmap SelfRef . toList . R.allowedWikiLinkTargets . noteRoute

noteTitle :: Note -> Text
noteTitle Note {..} =
  fromMaybe (R.markdownRouteFileBase noteRoute) $ PandocUtil.getPandocTitle noteDoc

type NoteIxs = '[MarkdownRoute, SelfRef]

type IxNote = IxSet NoteIxs Note

instance Indexable NoteIxs Note where
  indices =
    ixList
      (ixGen $ Proxy @MarkdownRoute)
      (ixFun noteSelfRefs)

-- | A relation from one note to another.
data Rel = Rel
  { relFrom :: MarkdownRoute,
    relTo :: Either R.WikiLinkTarget R.MarkdownRoute,
    -- | The relation context of 'from' note linking to 'to' note.
    relCtx :: NonEmpty [B.Block]
  }
  deriving (Data, Show)

instance Eq Rel where
  a == b =
    let f = relFrom &&& relTo
     in f a == f b

instance Ord Rel where
  a <= b =
    let f = relFrom &&& relTo
     in f a <= f b

type RelIxs = '[MarkdownRoute, Either R.WikiLinkTarget R.MarkdownRoute]

type IxRel = IxSet RelIxs Rel

instance Indexable RelIxs Rel where
  indices =
    ixList
      (ixGen $ Proxy @MarkdownRoute)
      (ixGen $ Proxy @(Either R.WikiLinkTarget R.MarkdownRoute))

extractRels :: Note -> [Rel]
extractRels note =
  extractLinks . Map.map (fmap snd) . LC.queryLinksWithContext . noteDoc $ note
  where
    extractLinks :: Map Text (NonEmpty [B.Block]) -> [Rel]
    extractLinks m =
      flip mapMaybe (Map.toList m) $ \(url, ctx) -> do
        target <- parseUrl url
        pure $ Rel (noteRoute note) target ctx

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

modelLookupBacklinks :: MarkdownRoute -> Model -> [(MarkdownRoute, NonEmpty [B.Block])]
modelLookupBacklinks r model =
  let refsToSelf =
        Set.fromList $
          (Left <$> toList (R.allowedWikiLinkTargets r))
            <> [Right r]
      backlinks = Ix.toList $ modelRels model @+ toList refsToSelf
   in backlinks <&> \rel ->
        (relFrom rel, relCtx rel)

modelLookupTitle :: MarkdownRoute -> Model -> Text
modelLookupTitle r =
  maybe (R.markdownRouteFileBase r) noteTitle . modelLookup r

modelUpdateSettings :: FilePath -> Text -> Model -> Model
modelUpdateSettings settingsFile s model =
  model
    { modelSettings =
        traceShowId $
          either error Aeson.toJSON $
            parseYaml @(Y.Node Y.Pos) settingsFile s
    }

modelInsert :: MarkdownRoute -> (Meta, Pandoc) -> Model -> Model
modelInsert k v model =
  let note = Note (snd v) (fst v) k
      modelNotes' =
        modelNotes model
          & Ix.updateIx k note
      modelRels' =
        modelRels model
          & Ix.deleteIx k
          & Ix.insertList (extractRels note)
   in model
        { modelNotes = modelNotes',
          modelRels = modelRels',
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
      modelRels = Ix.deleteIx k (modelRels model),
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
    & PandocUtil.rewriteLinksM resolveUrl
  where
    -- Convert .md or wiki links to their proper route url.
    --
    -- Requires resolution from the `model` state. Late resolution, in other words.
    resolveUrl url =
      fmap (fromMaybe url) . runMaybeT @m $ do
        guard $ not $ isStaticAssetUrl url
        hoistMaybe (parseUrl url) >>= \case
          Right r -> do
            pure $ Ema.routeUrl r
          Left wl ->
            case nonEmpty (modelLookupRouteByWikiLink wl model) of
              Nothing -> do
                tell $ one (docRoute, url)
                -- TODO: Set an attribute for broken links, so templates can style it accordingly
                -- TODO: Return url as is, so backlinks work?
                pure "/EmaNotFound"
              Just targets ->
                -- TODO: Deal with ambiguous targets here
                pure $ Ema.routeUrl $ head targets
    isStaticAssetUrl url =
      any (\asset -> ("/" <> toText asset) `T.isPrefixOf` url) (staticAssets $ Proxy @MarkdownRoute)

-- | Parse a URL string
parseUrl :: Text -> Maybe (Either R.WikiLinkTarget MarkdownRoute)
parseUrl url = do
  guard $ not $ "://" `T.isInfixOf` url
  fmap Left (R.mkWikiLinkTargetFromUrl url)
    <|> fmap Right (R.mkMarkdownRouteFromFilePath $ toString url)
