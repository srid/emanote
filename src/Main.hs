{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception (throw)
import Control.Monad.Logger
import Data.Aeson (ToJSON (toJSON))
import Data.Default (Default (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Profunctor (dimap)
import qualified Data.Text as T
import Data.Tree (Tree (Node))
import qualified Data.YAML as Y
import Ema (Ema (..), Slug)
import qualified Ema
import qualified Ema.CLI
import qualified Ema.Helper.FileSystem as FileSystem
import qualified Ema.Helper.Markdown as Markdown
import qualified Ema.Helper.PathTree as PathTree
import NeatInterpolation (text)
import System.FilePath (splitExtension, splitPath)
import qualified Text.Blaze.Html.Renderer.Utf8 as RU
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Ginger as G
import qualified Text.Ginger.Html as G
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))
import qualified Text.Pandoc.Walk as W

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
    modelTemplate :: Either G.ParserError (G.Template G.SourcePos)
  }
  deriving (Show)

instance Default Model where
  def = Model mempty mempty (Left $ G.ParserError "Uninitialized" Nothing)

data Meta = Meta
  { -- | Indicates the order of the Markdown file in sidebar tree, relative to
    -- its siblings.
    order :: Word,
    tags :: [Text]
  }
  deriving (Eq, Show)

instance Y.FromYAML Meta where
  parseYAML = Y.withMap "FrontMatter" $ \m ->
    Meta
      <$> m Y..: "order"
      <*> m Y..: "tags"

instance Default Meta where
  def = Meta maxBound mempty

data NoteContext = NoteContext
  { html :: Text,
    title :: Text,
    -- TODO: These should be defined in templates, with ony data passed over
    sidebarHtml :: Text,
    breadcrumbsHtml :: Text
  }
  deriving (Generic, ToJSON)

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

modelSetTemplate :: Either G.ParserError (G.Template G.SourcePos) -> Model -> Model
modelSetTemplate v model =
  model {modelTemplate = v}

modelTemplateRender :: Model -> NoteContext -> Text
modelTemplateRender model noteContext =
  let ctxLookup var =
        if var == "note" then G.toGVal (toJSON noteContext) else def
      ctx = G.makeContextHtml ctxLookup
      tmpl = either (error . show) id $ modelTemplate model
   in G.htmlSource $ G.runGinger ctx tmpl

modelDelete :: MarkdownRoute -> Model -> Model
modelDelete k model =
  model
    { modelDocs = Map.delete k (modelDocs model),
      modelNav = PathTree.treeDeletePath (unMarkdownRoute k) (modelNav model)
    }

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
  staticAssets _ =
    ["favicon.jpeg"]

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
    FileSystem.mountOnLVar "." ["**/*.md", templateFile] model $ \fp action -> do
      case snd $ splitExtension fp of
        ".md" -> case action of
          FileSystem.Update -> do
            mData <- readSource fp
            pure $ maybe id (uncurry modelInsert) mData
          FileSystem.Delete ->
            pure $ maybe id modelDelete (mkMarkdownRouteFromFilePath fp)
        _ -> do
          if fp /= templateFile
            then pure id
            else case action of
              FileSystem.Delete ->
                pure $ modelSetTemplate $ Left $ G.ParserError "No template found" Nothing
              FileSystem.Update ->
                modelSetTemplate <$> G.parseGingerFile (fmap Just . readFile) fp
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

-- ------------------------
-- Our site HTML
-- ------------------------

render :: Ema.CLI.Action -> Model -> MarkdownRoute -> LByteString
render emaAction model r = do
  case modelLookup r model of
    Nothing ->
      -- In dev server mode, Ema will display the exceptions in the browser.
      -- In static generation mode, they will cause the generation to crash.
      throw $ BadRoute r
    Just doc -> do
      -- You can return your own HTML string here, but we use the Tailwind+Blaze helper
      let ctx =
            NoteContext
              { html = decodeUtf8 . RU.renderHtml $ renderMarkdownAfterVerify model doc,
                title =
                  if r == indexMarkdownRoute
                    then "emabook Notebook"
                    else lookupTitle doc r,
                sidebarHtml =
                  decodeUtf8 . RU.renderHtml $ renderSidebarNav model r,
                breadcrumbsHtml =
                  decodeUtf8 . RU.renderHtml $ renderBreadcrumbs model r
              }
      encodeUtf8 $ modelTemplateRender model ctx

renderMarkdownAfterVerify :: Model -> Pandoc -> H.Html
renderMarkdownAfterVerify model doc =
  renderPandoc $
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

renderSidebarNav :: Model -> MarkdownRoute -> H.Html
renderSidebarNav model currentRoute = do
  -- Drop toplevel index.md from sidebar tree (because we are linking to it manually)
  let navTree = PathTree.treeDeleteChild "index" $ modelNav model
  go [] navTree
  where
    go parSlugs xs =
      H.div ! A.class_ "pl-2" $ do
        forM_ xs $ \(Node slug children) -> do
          let hereRoute = MarkdownRoute $ NE.reverse $ slug :| parSlugs
          renderRoute (if null parSlugs || not (null children) then "" else "text-gray-600") hereRoute
          go ([slug] <> parSlugs) children
    renderRoute c r = do
      let linkCls = if r == currentRoute then "text-pink-600 font-bold" else ""
      H.div ! A.class_ ("my-2 " <> c) $ H.a ! A.class_ (" hover:text-black  " <> linkCls) ! A.href (H.toValue $ Ema.routeUrl r) $ H.toHtml $ lookupTitleForgiving model r

renderBreadcrumbs :: Model -> MarkdownRoute -> H.Html
renderBreadcrumbs model r = do
  whenNotNull (init $ markdownRouteInits r) $ \(toList -> crumbs) ->
    H.div ! A.class_ "w-full text-gray-600 mt-4 block md:hidden" $ do
      H.div ! A.class_ "flex justify-center" $ do
        H.div ! A.class_ "w-full bg-white py-2 rounded" $ do
          H.ul ! A.class_ "flex text-gray-500 text-sm lg:text-base" $ do
            forM_ crumbs $ \crumb ->
              H.li ! A.class_ "inline-flex items-center" $ do
                H.a ! A.class_ "px-1 font-bold bg-pink-500 text-gray-50 rounded"
                  ! A.href (fromString . toString $ Ema.routeUrl crumb)
                  $ H.text $ lookupTitleForgiving model crumb
                rightArrow
            H.li ! A.class_ "inline-flex items-center text-gray-600" $ do
              H.a $ H.text $ lookupTitleForgiving model r
  where
    rightArrow =
      H.unsafeByteString $
        encodeUtf8
          [text|
          <svg fill="currentColor" viewBox="0 0 20 20" class="h-5 w-auto text-gray-400"><path fill-rule="evenodd" d="M7.293 14.707a1 1 0 010-1.414L10.586 10 7.293 6.707a1 1 0 011.414-1.414l4 4a1 1 0 010 1.414l-4 4a1 1 0 01-1.414 0z" clip-rule="evenodd"></path></svg>
          |]

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
-- Pandoc renderer
-- ------------------------
--
-- Note that we hardcode tailwind classes, because pandoc AST is not flexible
-- enough to provide attrs for all inlines/blocks. So we can't rely on Walk to
-- transform it.

renderPandoc :: Pandoc -> H.Html
renderPandoc (Pandoc _meta blocks) =
  mapM_ rpBlock blocks

rpBlock :: B.Block -> H.Html
rpBlock = \case
  B.Plain is ->
    mapM_ rpInline is
  B.Para is ->
    H.p ! A.class_ "my-2" $ mapM_ rpInline is
  B.LineBlock iss ->
    forM_ iss $ \is ->
      mapM_ rpInline is >> "\n"
  B.CodeBlock (id', classes, attrs) s ->
    -- Prism friendly classes
    let classes' = flip concatMap classes $ \cls -> [cls, "language-" <> cls]
     in H.div ! A.class_ "py-0.5 text-sm" $ H.pre ! rpAttr (id', classes', attrs) $ H.code ! rpAttr ("", classes', []) $ H.text s
  B.RawBlock (B.Format fmt) rawHtml ->
    if fmt == "html"
      then H.unsafeByteString $ encodeUtf8 rawHtml
      else throw Unsupported
  B.BlockQuote bs ->
    H.blockquote $ mapM_ rpBlock bs
  B.OrderedList _ bss ->
    H.ol ! A.class_ (listStyle <> " list-decimal") $
      forM_ bss $ \bs ->
        H.li ! A.class_ listItemStyle $ mapM_ rpBlock bs
  B.BulletList bss ->
    H.ul ! A.class_ (listStyle <> " list-disc") $
      forM_ bss $ \bs ->
        H.li ! A.class_ listItemStyle $ mapM_ rpBlock bs
  B.DefinitionList defs ->
    H.dl $
      forM_ defs $ \(term, descList) -> do
        mapM_ rpInline term
        forM_ descList $ \desc ->
          H.dd $ mapM_ rpBlock desc
  B.Header level attr is ->
    headerElem level ! rpAttr attr $ mapM_ rpInline is
  B.HorizontalRule ->
    H.hr
  B.Table {} ->
    throw Unsupported
  B.Div attr bs ->
    H.div ! rpAttr attr $ mapM_ rpBlock bs
  B.Null ->
    pure ()
  where
    listStyle = "list-inside ml-4 space-y-1 "
    listItemStyle = ""

headerElem :: Int -> H.Html -> H.Html
headerElem = \case
  1 -> H.h1 ! A.class_ "text-6xl mt-2 mb-2 text-center pb-2"
  2 -> H.h2 ! A.class_ ("text-5xl " <> my)
  3 -> H.h3 ! A.class_ ("text-4xl " <> my)
  4 -> H.h4 ! A.class_ ("text-3xl " <> my)
  5 -> H.h5 ! A.class_ ("text-2xl " <> my)
  6 -> H.h6 ! A.class_ ("text-xl " <> my)
  _ -> error "Invalid pandoc header level"
  where
    my = "mt-4 mb-2 text-gray-700"

rpInline :: B.Inline -> H.Html
rpInline = \case
  B.Str s -> H.toHtml s
  B.Emph is ->
    H.em $ mapM_ rpInline is
  B.Strong is ->
    H.strong $ mapM_ rpInline is
  B.Underline is ->
    H.u $ mapM_ rpInline is
  B.Strikeout is ->
    -- FIXME: Should use <s>, but blaze doesn't have it.
    H.del $ mapM_ rpInline is
  B.Superscript is ->
    H.sup $ mapM_ rpInline is
  B.Subscript is ->
    H.sub $ mapM_ rpInline is
  B.Quoted qt is ->
    flip inQuotes qt $ mapM_ rpInline is
  B.Code attr s ->
    H.code ! rpAttr attr $ H.toHtml s
  B.Space -> " "
  B.SoftBreak -> " "
  B.LineBreak -> H.br
  B.RawInline _fmt s ->
    H.pre $ H.toHtml s
  B.Math _ _ ->
    throw Unsupported
  B.Link attr is (url, tit) -> do
    let (cls, target) =
          if "://" `T.isInfixOf` url
            then ("text-pink-600 hover:underline", targetBlank)
            else ("text-pink-600 font-bold hover:bg-pink-50", mempty)
    H.a
      ! A.class_ cls
      ! A.href (H.textValue url)
      ! A.title (H.textValue tit)
      ! target
      ! rpAttr attr
      $ mapM_ rpInline is
  B.Image attr is (url, tit) ->
    H.img ! A.src (H.textValue url) ! A.title (H.textValue tit) ! A.alt (H.textValue $ Markdown.plainify is) ! rpAttr attr
  B.Note _ ->
    throw Unsupported
  B.Span attr is ->
    H.span ! rpAttr attr $ mapM_ rpInline is
  x ->
    H.pre $ H.toHtml $ show @Text x
  where
    inQuotes :: H.Html -> B.QuoteType -> H.Html
    inQuotes w = \case
      B.SingleQuote -> "‘" >> w <* "’"
      B.DoubleQuote -> "“" >> w <* "”"

targetBlank :: H.Attribute
targetBlank =
  A.target "_blank" <> A.rel "noopener"

rpAttr :: B.Attr -> H.Attribute
rpAttr (id', classes, attrs) =
  let cls = T.intercalate " " classes
   in unlessNull id' (A.id (fromString . toString $ id'))
        <> unlessNull cls (A.class_ (fromString . toString $ cls))
        <> mconcat (fmap (\(k, v) -> H.dataAttribute (fromString . toString $ k) (fromString . toString $ v)) attrs)
  where
    unlessNull x f =
      if T.null x then mempty else f

data Unsupported = Unsupported
  deriving (Show, Exception)

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
