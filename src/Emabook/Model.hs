{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Emabook.Model where

import Control.Monad.Writer.Strict (MonadWriter (tell))
import Data.Data (Data)
import Data.Default (Default (..))
import Data.IxSet (Indexable (..), IxSet, ixFun, ixGen, ixSet, (@=))
import qualified Data.IxSet as Ix
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

-- | This is our Ema "model" -- the app state used to generate our site.
--
-- It contains the list of all markdown files, parsed as Pandoc AST.
data Model = Model
  { modelNotes :: IxSet Note,
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
  deriving (Eq, Ord, Data)

-- | Wiki-links that refer to this note.
noteWikiLinks :: Note -> [R.WikiLinkTarget]
noteWikiLinks = toList . R.allowedWikiLinkTargets . noteRoute

instance Indexable Note where
  empty =
    ixSet
      [ ixGen (Ix.Proxy :: Ix.Proxy MarkdownRoute),
        ixFun noteWikiLinks
      ]

data Meta = Meta
  { -- | Indicates the order of the Markdown file in sidebar tree, relative to
    -- its siblings. Default value: 0.
    order :: Maybe Int,
    tags :: Maybe [Text]
  }
  deriving (Eq, Show, Ord, Data)

instance Y.FromYAML Meta where
  parseYAML = Y.withMap "FrontMatter" $ \m ->
    Meta
      <$> m Y..:? "order"
      <*> m Y..:? "tags"

instance Default Meta where
  def = Meta Nothing Nothing

modelLookup :: MarkdownRoute -> Model -> Maybe Note
modelLookup k =
  Ix.getOne . Ix.getEQ k . modelNotes

modelLookupMeta :: MarkdownRoute -> Model -> Meta
modelLookupMeta k =
  maybe def noteMeta . modelLookup k

modelLookupWikiLink :: R.WikiLinkTarget -> Model -> [MarkdownRoute]
modelLookupWikiLink wl model =
  fmap noteRoute . Ix.toList $ modelNotes model @= wl

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
    sortKey notes r = fromMaybe (0, R.markdownRouteFileBase r) $ do
      note <- Ix.getOne $ Ix.getEQ r notes
      let docOrder = fromMaybe 0 $ order $ noteMeta note
      pure (docOrder, docTitle r $ noteDoc note)

-- | Return title associated with the given route.
--
-- Prefer Pandoc titallowedWikiLinkTargetsle if the Markdown file exists, otherwise return the file's basename.
routeTitle :: MarkdownRoute -> Model -> Text
routeTitle r =
  maybe (R.markdownRouteFileBase r) (docTitle r . noteDoc) . modelLookup r

-- | Return title of the given `Pandoc`. If there is no title, use the route to determine the title.
docTitle :: MarkdownRoute -> Pandoc -> Text
docTitle r =
  fromMaybe (R.markdownRouteFileBase r) . PandocUtil.getPandocTitle

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
  MonadWriter [(MarkdownRoute, Text)] m =>
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
      let reportBrokenLink r s = tell $ one @[(MarkdownRoute, Text)] (r, s)
      PandocUtil.rewriteRelativeLinksM $ \url -> do
        if any (\asset -> ("/" <> toText asset) `T.isPrefixOf` url) (staticAssets $ Proxy @MarkdownRoute)
          then -- This is a link to a static asset
            pure url
          else case R.mkWikiLinkTargetFromUrl url of
            Nothing ->
              -- Not a wiki link
              pure url
            Just wl ->
              case nonEmpty (modelLookupWikiLink wl model) of
                Nothing -> do
                  reportBrokenLink docRoute url
                  -- TODO: Set an attribute for broken links, so templates can style it accordingly
                  pure "/EmaNotFound"
                Just targets ->
                  -- TODO: Deal with ambiguous targets here
                  pure $ Ema.routeUrl $ head targets
    -- Resolve Bar/Foo.md
    rewriteMdLinks =
      PandocUtil.rewriteRelativeLinks $ \url -> fromMaybe url $ do
        guard $ ".md" `T.isSuffixOf` url
        r <- R.mkMarkdownRouteFromFilePath $ toString url
        pure $ Ema.routeUrl r
