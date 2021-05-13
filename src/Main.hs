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

-- ------------------------
-- Our site rendering
-- ------------------------

render :: Ema.CLI.Action -> Model -> MarkdownRoute -> LByteString
render _ model r = do
  case M.modelLookup r model of
    Nothing ->
      throw $ R.BadRoute r
    Just doc -> do
      -- TODO: Look for "${r}" template, and then fallback to _default
      flip (T.renderHeistTemplate "_default") (M.modelHeistTemplate model) $ do
        -- Common stuff
        "theme" ## HI.textSplice "yellow"
        -- Nav stuff
        "ema:route-tree"
          ## ( let tree = PathTree.treeDeleteChild "index" $ M.modelNav model
                in Splices.treeSplice tree r R.MarkdownRoute $ H.toHtml . flip routeTitle model
             )
        "ema:breadcrumbs" ## Splices.listSplice (init $ R.markdownRouteInits r) "crumb" $ \crumb ->
          MapSyntax.mapV HI.textSplice $ do
            "crumb:url" ## Ema.routeUrl crumb
            "crumb:title" ## routeTitle crumb model
        -- Note stuff
        "ema:note:title" ## HI.textSplice $
          if r == R.indexMarkdownRoute
            then "emabook"
            else docTitle r doc
        "ema:note:pandoc" ## Splices.pandocSplice (sanitizeMarkdown model doc)

-- | Return title associated with the given route.
--
-- Prefer Pandoc title if the Markdown file exists, otherwise return the file's basename.
routeTitle :: MarkdownRoute -> Model -> Text
routeTitle r =
  maybe (R.markdownRouteFileBase r) (docTitle r) . M.modelLookup r

-- | Return title of the given `Pandoc`. If there is no title, use the route to determine the title.
docTitle :: MarkdownRoute -> Pandoc -> Text
docTitle r =
  fromMaybe (R.markdownRouteFileBase r) . getPandocTitle

sanitizeMarkdown :: Model -> Pandoc -> Pandoc
sanitizeMarkdown _model doc =
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
          -- TODO: Not doing this, until determining how to handle non-existant wiki-links
          {- if modelMember target model
            then pure $ Ema.routeUrl target
            else throw $ BadRoute target -}
      )

-- ------------------------
-- Pandoc AST helpers
-- ------------------------

getPandocTitle :: Pandoc -> Maybe Text
getPandocTitle =
  fmap Markdown.plainify . getPandocH1

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

rewriteLinks :: (Text -> Text) -> Pandoc -> Pandoc
rewriteLinks f =
  W.walk $ \case
    B.Link attr is (url, tit) ->
      B.Link attr is (f url, tit)
    x -> x
