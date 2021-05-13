{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Emabook.Model where

import Data.Default (Default (..))
import qualified Data.Map.Strict as Map
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

-- ------------------------
-- Our site model
-- ------------------------

-- | This is our Ema "model" -- the app state used to generate our site.
--
-- It contains the list of all markdown files, parsed as Pandoc AST.
data Model = Model
  { modelDocs :: Map MarkdownRoute (Meta, Pandoc),
    modelNav :: [Tree Slug],
    modelHeistTemplate :: T.TemplateState
  }

instance Default Model where
  def = Model mempty mempty (Left $ one "Heist state not yet loaded")

data Meta = Meta
  { -- | Indicates the order of the Markdown file in sidebar tree, relative to
    -- its siblings. Default value: 0.
    order :: Maybe Int,
    tags :: Maybe [Text]
  }
  deriving (Eq, Show)

instance Y.FromYAML Meta where
  parseYAML = Y.withMap "FrontMatter" $ \m ->
    Meta
      <$> m Y..:? "order"
      <*> m Y..:? "tags"

instance Default Meta where
  def = Meta Nothing Nothing

modelLookup :: MarkdownRoute -> Model -> Maybe Pandoc
modelLookup k =
  fmap snd . Map.lookup k . modelDocs

-- | Like `modelLookup` but looks up Markdown by the base filename (without extension) only.
--
-- Assumes that all Markdown file names, across folder hierarchy, are unique.
modelLookupFileName :: Text -> Model -> Maybe MarkdownRoute
modelLookupFileName name (modelDocs -> m) =
  -- TODO: Instead of listToMaybe, handle ambiguous notes properly.
  listToMaybe $
    Map.keys $
      flip Map.filterWithKey m $ \r _ ->
        name == R.markdownRouteFileBase r

modelLookupMeta :: MarkdownRoute -> Model -> Meta
modelLookupMeta k =
  maybe def fst . Map.lookup k . modelDocs

modelMember :: MarkdownRoute -> Model -> Bool
modelMember k =
  Map.member k . modelDocs

modelInsert :: MarkdownRoute -> (Meta, Pandoc) -> Model -> Model
modelInsert k v model =
  let modelDocs' = Map.insert k v (modelDocs model)
   in model
        { modelDocs = modelDocs',
          modelNav =
            PathTree.treeInsertPathMaintainingOrder
              (sortKey modelDocs' . R.MarkdownRoute)
              (R.unMarkdownRoute k)
              (modelNav model)
        }
  where
    -- Sort by `order` meta, falling back to title.
    sortKey modelDocs' r = fromMaybe (0, R.markdownRouteFileBase r) $ do
      (meta, doc) <- Map.lookup r modelDocs'
      let docOrder = fromMaybe 0 $ order meta
      pure (docOrder, docTitle r doc)

-- | Return title associated with the given route.
--
-- Prefer Pandoc title if the Markdown file exists, otherwise return the file's basename.
routeTitle :: MarkdownRoute -> Model -> Text
routeTitle r =
  maybe (R.markdownRouteFileBase r) (docTitle r) . modelLookup r

-- | Return title of the given `Pandoc`. If there is no title, use the route to determine the title.
docTitle :: MarkdownRoute -> Pandoc -> Text
docTitle r =
  fromMaybe (R.markdownRouteFileBase r) . PandocUtil.getPandocTitle

modelDelete :: MarkdownRoute -> Model -> Model
modelDelete k model =
  model
    { modelDocs = Map.delete k (modelDocs model),
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
  staticRoutes (Map.keys . modelDocs -> mdRoutes) =
    mdRoutes

  -- All static assets (relative to input directory) go here.
  -- Not all of these may exist.
  staticAssets _ =
    ["favicon.jpeg", "favicon.svg", "static"]
