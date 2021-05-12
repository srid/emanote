{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception (throw)
import Control.Monad.Logger
import Data.Default (Default (..))
import Data.List (isInfixOf)
import Data.Map.Syntax ((##))
import qualified Data.Map.Syntax as MapSyntax
import qualified Data.Text as T
import qualified Ema
import qualified Ema.CLI
import qualified Ema.Helper.FileSystem as FileSystem
import qualified Ema.Helper.Markdown as Markdown
import qualified Ema.Helper.PathTree as PathTree
import Emabook.Model (Model)
import qualified Emabook.Model as M
import Emabook.Route (MarkdownRoute)
import qualified Emabook.Route as R
import qualified Emabook.Template as T
import qualified Emabook.Template.Splices.List as Splices
import qualified Emabook.Template.Splices.Pandoc as Splices
import qualified Emabook.Template.Splices.Tree as Splices
import Heist (Splices)
import qualified Heist.Interpreted as HI
import System.FilePath (splitExtension, (</>))
import qualified Text.Blaze.Html5 as H
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))
import qualified Text.Pandoc.Walk as W

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
            pure $ maybe id (uncurry M.modelInsert) mData
          FileSystem.Delete ->
            pure $ maybe id M.modelDelete (R.mkMarkdownRouteFromFilePath fp)
        _ -> do
          if heistTemplateDir `isInfixOf` fp
            then M.modelSetHeistTemplate <$> T.loadHeistTemplates heistTemplateDir
            else pure id
  where
    readSource :: (MonadIO m, MonadLogger m) => FilePath -> m (Maybe (MarkdownRoute, (M.Meta, Pandoc)))
    readSource fp =
      runMaybeT $ do
        r :: MarkdownRoute <- MaybeT $ pure $ R.mkMarkdownRouteFromFilePath fp
        logD $ "Reading " <> toText fp
        s <- readFileText fp
        pure (r, either (throw . BadMarkdown) (first $ fromMaybe def) $ parseMarkdown fp s)
    parseMarkdown =
      Markdown.parseMarkdownWithFrontMatter @M.Meta $ Markdown.wikilinkSpec <> Markdown.fullMarkdownSpec

newtype BadMarkdown = BadMarkdown Text
  deriving (Show, Exception)

data NoteContext = NoteContext
  { title :: Text,
    doc :: Pandoc,
    here :: MarkdownRoute,
    model :: Model
  }

mkNoteContext :: Model -> MarkdownRoute -> NoteContext
mkNoteContext model r =
  case M.modelLookup r model of
    Nothing ->
      throw $ R.BadRoute r
    Just doc -> do
      NoteContext
        { doc = doc,
          title =
            if r == R.indexMarkdownRoute
              then -- TODO: Configurable site title (via heist splice?)
                "emabook"
              else lookupTitle doc r,
          here = r,
          model = model
        }

noteContextSplices :: forall n. Monad n => NoteContext -> Heist.Splices (HI.Splice n)
noteContextSplices ctx = do
  "note-title" ## HI.textSplice (title ctx)
  "note-pandoc" ## Splices.pandocSplice (verifyMarkdown (model ctx) (doc ctx))
  -- TODO: Should be in global context?
  "route-tree"
    ## ( let tree = PathTree.treeDeleteChild "index" $ M.modelNav $ model ctx
          in Splices.treeSplice tree (here ctx) R.MarkdownRoute $ H.toHtml . lookupTitleForgiving (model ctx)
       )
  "breadcrumbs"
    ## ( let crumbs = init $ R.markdownRouteInits $ here ctx
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
      splices = do
        noteContextSplices ctx
        "theme" ## HI.textSplice "yellow"
  T.renderHeistTemplate "_default" splices (M.modelHeistTemplate model)

verifyMarkdown :: Model -> Pandoc -> Pandoc
verifyMarkdown model doc =
  doc
    & withoutH1 -- Eliminate H1, because we are rendering it separately (see above)
    & rewriteLinks
      -- Rewrite .md links to @MarkdownRoute@
      ( \url -> fromMaybe url $ do
          guard $ not $ "://" `T.isInfixOf` url
          -- FIXME: Because wikilink parser returns "Foo.md", we must locate
          -- it and link to correct place in hierarchy.
          -- When doing this, bail out early on ambiguities.
          target <- R.mkMarkdownRouteFromUrl url
          pure $ Ema.routeUrl target
          -- Check that .md links are not broken
          {- if modelMember target model
            then pure $ Ema.routeUrl target
            else throw $ BadRoute target -}
      )

-- | This accepts if "${folder}.md" doesn't exist, and returns "folder" as the
-- title.
lookupTitleForgiving :: Model -> MarkdownRoute -> Text
lookupTitleForgiving model r =
  fromMaybe (R.markdownRouteFileBase r) $ do
    doc <- M.modelLookup r model
    is <- getPandocH1 doc
    pure $ Markdown.plainify is

lookupTitle :: Pandoc -> MarkdownRoute -> Text
lookupTitle doc r =
  maybe (Ema.unSlug $ last $ R.unMarkdownRoute r) Markdown.plainify $ getPandocH1 doc

-- ------------------------
-- Pandoc transformer
-- ------------------------

rewriteLinks :: (Text -> Text) -> Pandoc -> Pandoc
rewriteLinks f =
  W.walk $ \case
    B.Link attr is (url, tit) ->
      B.Link attr is (f url, tit)
    x -> x

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
