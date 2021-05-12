{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception (throw)
import Control.Monad.Logger
import Data.Default (Default (..))
import Data.List (isInfixOf)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Map.Syntax ((##))
import qualified Data.Map.Syntax as MapSyntax
import Data.Profunctor (dimap)
import qualified Data.Text as T
import Data.Tree (Tree)
import qualified Data.YAML as Y
import Ema (Ema (..), Slug)
import qualified Ema
import qualified Ema.CLI
import qualified Ema.Helper.FileSystem as FileSystem
import qualified Ema.Helper.Markdown as Markdown
import qualified Ema.Helper.PathTree as PathTree
import qualified Emabook.Template as T
import qualified Emabook.Template.Splices.List as Splices
import qualified Emabook.Template.Splices.Pandoc as Splices
import qualified Emabook.Template.Splices.Tree as Splices
import Heist (Splices)
import qualified Heist.Interpreted as HI
import System.FilePath (splitExtension, splitPath, (</>))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Renderer.XmlHtml as RX
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))
import qualified Text.Pandoc.Walk as W
import qualified Text.XmlHtml as XmlHtml

-- ------------------------
-- Our site route
-- ------------------------

-- | Represents the relative path to a source (.md) file under some directory.
--
-- This will also be our site route type.  That is, `Ema.routeUrl (r ::
-- MarkdownRoute)` gives us the URL to the generated HTML for this markdown file.
--
-- If you are using this repo as a template, you might want to use an ADT as
-- route (eg: data Route = Index | About)
newtype MarkdownRoute = MarkdownRoute {unMarkdownRoute :: NonEmpty Slug}
  deriving (Eq, Ord, Show)

newtype BadRoute = BadRoute MarkdownRoute
  deriving (Show, Exception)

-- | Represents the top-level index.md
indexMarkdownRoute :: MarkdownRoute
indexMarkdownRoute = MarkdownRoute $ "index" :| []

-- | Convert foo/bar.md to a @MarkdownRoute@
--
-- If the file is not a Markdown file, return Nothing.
mkMarkdownRouteFromFilePath :: FilePath -> Maybe MarkdownRoute
mkMarkdownRouteFromFilePath = \case
  (splitExtension -> (fp, ".md")) ->
    let slugs = fromString . toString . T.dropWhileEnd (== '/') . toText <$> splitPath fp
     in MarkdownRoute <$> nonEmpty slugs
  _ ->
    Nothing

mkMarkdownRouteFromUrl :: Text -> Maybe MarkdownRoute
mkMarkdownRouteFromUrl url =
  if ".md" `T.isSuffixOf` url
    then -- Regular Markdown link: [foo](foo.md); delegate.
      mkMarkdownRouteFromFilePath $ toString url
    else -- Expecting wikilink: [[foo]].

      let resolvedPath = url <> ".md" -- TODO: Resolve to correct directory
       in mkMarkdownRouteFromFilePath $ toString resolvedPath

markdownRouteSourcePath :: MarkdownRoute -> FilePath
markdownRouteSourcePath r =
  if r == indexMarkdownRoute
    then "index.md"
    else toString (T.intercalate "/" $ fmap Ema.unSlug $ toList $ unMarkdownRoute r) <> ".md"

-- | Filename of the markdown file without extension
markdownRouteFileBase :: MarkdownRoute -> Text
markdownRouteFileBase =
  Ema.unSlug . head . NE.reverse . unMarkdownRoute

-- | For use in breadcrumbs
markdownRouteInits :: MarkdownRoute -> NonEmpty MarkdownRoute
markdownRouteInits (MarkdownRoute ("index" :| [])) =
  one indexMarkdownRoute
markdownRouteInits (MarkdownRoute (slug :| rest')) =
  indexMarkdownRoute :| case nonEmpty rest' of
    Nothing ->
      one $ MarkdownRoute (one slug)
    Just rest ->
      MarkdownRoute (one slug) : go (one slug) rest
  where
    go :: NonEmpty Slug -> NonEmpty Slug -> [MarkdownRoute]
    go x (y :| ys') =
      let this = MarkdownRoute (x <> one y)
       in case nonEmpty ys' of
            Nothing ->
              one this
            Just ys ->
              this : go (unMarkdownRoute this) ys

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
    -- its siblings.
    order :: Word,
    tags :: Maybe [Text]
  }
  deriving (Eq, Show)

instance Y.FromYAML Meta where
  parseYAML = Y.withMap "FrontMatter" $ \m ->
    Meta
      <$> m Y..: "order"
      <*> m Y..:? "tags"

instance Default Meta where
  def = Meta maxBound mempty

modelLookup :: MarkdownRoute -> Model -> Maybe Pandoc
modelLookup k =
  fmap snd . Map.lookup k . modelDocs

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
              (\k' -> order $ maybe def fst $ Map.lookup (MarkdownRoute k') modelDocs')
              (unMarkdownRoute k)
              (modelNav model)
        }

modelDelete :: MarkdownRoute -> Model -> Model
modelDelete k model =
  model
    { modelDocs = Map.delete k (modelDocs model),
      modelNav = PathTree.treeDeletePath (unMarkdownRoute k) (modelNav model)
    }

modelSetHeistTemplate :: T.TemplateState -> Model -> Model
modelSetHeistTemplate v model =
  model {modelHeistTemplate = v}

-- | Once we have a "model" and "route" (as defined above), we should define the
-- @Ema@ typeclass to tell Ema how to decode/encode our routes, as well as the
-- list of routes to generate the static site with.
instance Ema Model MarkdownRoute where
  -- Convert a route to URL slugs
  encodeRoute = \case
    MarkdownRoute ("index" :| []) -> mempty
    MarkdownRoute paths -> toList paths

  -- Parse our route from URL slugs
  --
  -- For eg., /foo/bar maps to slugs ["foo", "bar"], which in our app gets
  -- parsed as representing the route to /foo/bar.md.
  decodeRoute = \case
    (nonEmpty -> Nothing) ->
      pure $ MarkdownRoute $ one "index"
    (nonEmpty -> Just slugs) -> do
      -- Heuristic to let requests to static files (eg: favicon.ico) to pass through
      guard $ not (any (T.isInfixOf "." . Ema.unSlug) slugs)
      pure $ MarkdownRoute slugs

  -- Which routes to generate when generating the static HTML for this site.
  staticRoutes (Map.keys . modelDocs -> mdRoutes) =
    mdRoutes

  -- All static assets (relative to input directory) go here.
  -- Not all of these may exist.
  staticAssets _ =
    ["favicon.jpeg", "favicon.svg", "static"]

-- ------------------------
-- Main entry point
-- ------------------------

log :: MonadLogger m => Text -> m ()
log = logInfoNS "emabook"

logD :: MonadLogger m => Text -> m ()
logD = logDebugNS "emabook"

main :: IO ()
main =
  Ema.runEma render $ \model -> do
    let templateFile = ".emabook/template.html"
        heistTemplateDir = ".emabook/templates"
    FileSystem.mountOnLVar "." ["**/*.md", templateFile, heistTemplateDir </> "*.tpl"] model $ \fp action -> do
      case snd $ splitExtension fp of
        ".md" -> case action of
          FileSystem.Update -> do
            mData <- readSource fp
            pure $ maybe id (uncurry modelInsert) mData
          FileSystem.Delete ->
            pure $ maybe id modelDelete (mkMarkdownRouteFromFilePath fp)
        _ -> do
          if heistTemplateDir `isInfixOf` fp
            then modelSetHeistTemplate <$> T.loadHeistTemplates heistTemplateDir
            else pure id
  where
    readSource :: (MonadIO m, MonadLogger m) => FilePath -> m (Maybe (MarkdownRoute, (Meta, Pandoc)))
    readSource fp =
      runMaybeT $ do
        r :: MarkdownRoute <- MaybeT $ pure $ mkMarkdownRouteFromFilePath fp
        logD $ "Reading " <> toText fp
        s <- readFileText fp
        pure (r, either (throw . BadMarkdown) (first $ fromMaybe def) $ parseMarkdown fp s)
    parseMarkdown =
      Markdown.parseMarkdownWithFrontMatter @Meta $ Markdown.wikilinkSpec <> Markdown.fullMarkdownSpec

newtype BadMarkdown = BadMarkdown Text
  deriving (Show, Exception)

data NoteContext = NoteContext
  { html :: [XmlHtml.Node],
    title :: Text,
    here :: MarkdownRoute,
    model :: Model
  }

mkNoteContext :: Model -> MarkdownRoute -> NoteContext
mkNoteContext model r =
  case modelLookup r model of
    Nothing ->
      throw $ BadRoute r
    Just doc -> do
      NoteContext
        { html = renderHtml $ renderMarkdownAfterVerify model doc,
          title =
            if r == indexMarkdownRoute
              then -- TODO: Configurable site title (via heist splice?)
                "emabook"
              else lookupTitle doc r,
          here = r,
          model = model
        }

noteContextSplices :: forall n. Monad n => NoteContext -> Heist.Splices (HI.Splice n)
noteContextSplices ctx = do
  "note-title" ## HI.textSplice (title ctx)
  "note-html" ## pure (html ctx)
  -- TODO: Should be in global context?
  "route-tree"
    ## ( let tree = PathTree.treeDeleteChild "index" $ modelNav $ model ctx
          in Splices.treeSplice tree (here ctx) MarkdownRoute $ H.toHtml . lookupTitleForgiving (model ctx)
       )
  "breadcrumbs"
    ## ( let crumbs = init $ markdownRouteInits $ here ctx
          in Splices.listSplice crumbs "crumb" $ \crumb ->
               MapSyntax.mapV HI.textSplice $ do
                 "crumb-url" ## Ema.routeUrl crumb
                 "crumb-title" ## lookupTitleForgiving (model ctx) crumb
       )

-- ------------------------
-- Our site HTML
-- ------------------------

render :: Ema.CLI.Action -> Model -> MarkdownRoute -> LByteString
render _emaAction model r = do
  let ctx = mkNoteContext model r
      ctxSplices = noteContextSplices ctx
  T.renderHeistTemplate "_default" ctxSplices (modelHeistTemplate model)

renderHtml :: H.Html -> [XmlHtml.Node]
renderHtml h =
  case RX.renderHtml h of
    XmlHtml.HtmlDocument {..} ->
      docContent
    _ ->
      error "not a HTML document"

renderMarkdownAfterVerify :: Model -> Pandoc -> H.Html
renderMarkdownAfterVerify model doc =
  Splices.renderPandoc $
    doc
      & withoutH1 -- Eliminate H1, because we are rendering it separately (see above)
      & applyClassLibrary (\c -> fromMaybe c $ Map.lookup c emaMarkdownStyleLibrary)
      & rewriteLinks
        -- Rewrite .md links to @MarkdownRoute@
        ( \url -> fromMaybe url $ do
            guard $ not $ "://" `T.isInfixOf` url
            -- FIXME: Because wikilink parser returns "Foo.md", we must locate
            -- it and link to correct place in hierarchy.
            -- When doing this, bail out early on ambiguities.
            target <- mkMarkdownRouteFromUrl url
            pure $ Ema.routeUrl target
            -- Check that .md links are not broken
            {- if modelMember target model
              then pure $ Ema.routeUrl target
              else throw $ BadRoute target -}
        )
  where
    emaMarkdownStyleLibrary =
      Map.fromList
        [ ("feature", "flex justify-center items-center text-center shadow-lg p-2 m-2 w-32 h-16 lg:w-auto rounded border-2 border-gray-400 bg-pink-100 text-base font-bold hover:bg-pink-200 hover:border-black"),
          ("avatar", "float-right w-32 h-32"),
          -- List item specifc styles
          ("item-intro", "text-gray-500"),
          -- Styling the last line in series posts
          ("last", "mt-8 border-t-2 border-pink-500 pb-1 pl-1 bg-gray-50 rounded"),
          ("next", "py-2 text-xl italic font-bold")
        ]

-- | This accepts if "${folder}.md" doesn't exist, and returns "folder" as the
-- title.
lookupTitleForgiving :: Model -> MarkdownRoute -> Text
lookupTitleForgiving model r =
  fromMaybe (markdownRouteFileBase r) $ do
    doc <- modelLookup r model
    is <- getPandocH1 doc
    pure $ Markdown.plainify is

lookupTitle :: Pandoc -> MarkdownRoute -> Text
lookupTitle doc r =
  maybe (Ema.unSlug $ last $ unMarkdownRoute r) Markdown.plainify $ getPandocH1 doc

-- ------------------------
-- Pandoc transformer
-- ------------------------

rewriteLinks :: (Text -> Text) -> Pandoc -> Pandoc
rewriteLinks f =
  W.walk $ \case
    B.Link attr is (url, tit) ->
      B.Link attr is (f url, tit)
    x -> x

applyClassLibrary :: (Text -> Text) -> Pandoc -> Pandoc
applyClassLibrary f =
  walkBlocks . walkInlines
  where
    walkBlocks = W.walk $ \case
      B.Div attr bs ->
        B.Div (g attr) bs
      x -> x
    walkInlines = W.walk $ \case
      B.Span attr is ->
        B.Span (g attr) is
      x -> x
    g (id', cls, attr) =
      (id', withPackedClass f cls, attr)
    withPackedClass :: (Text -> Text) -> [Text] -> [Text]
    withPackedClass =
      dimap (T.intercalate " ") (T.splitOn " ")

-- ------------------------
-- Pandoc AST helpers
-- ------------------------

getPandocH1 :: Pandoc -> Maybe [B.Inline]
getPandocH1 = listToMaybe . W.query go
  where
    go :: B.Block -> [[B.Inline]]
    go = \case
      B.Header 1 _ inlines ->
        [inlines]
      _ ->
        []

withoutH1 :: Pandoc -> Pandoc
withoutH1 (Pandoc meta (B.Header 1 _ _ : rest)) =
  Pandoc meta rest
withoutH1 doc =
  doc
