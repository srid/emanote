{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception (throw)
import Control.Monad.Logger
import Data.Default (Default (..))
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
import qualified Emabook.PandocUtil as PandocUtil
import Emabook.Route (MarkdownRoute)
import qualified Emabook.Route as R
import qualified Emabook.Template as T
import qualified Emabook.Template.Splices.List as Splices
import qualified Emabook.Template.Splices.Pandoc as Splices
import qualified Emabook.Template.Splices.Tree as Splices
import qualified Heist.Interpreted as HI
import System.FilePath ((</>))
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

logE :: MonadLogger m => Text -> m ()
logE = logErrorNS "emabook"

data Source
  = SourceMarkdown
  | SourceTemplate FilePath
  deriving (Eq, Show)

sourcePattern :: Source -> FilePath
sourcePattern = \case
  SourceMarkdown -> "**/*.md"
  SourceTemplate dir -> dir </> "*.tpl"

main :: IO ()
main =
  Ema.runEma render $ \model -> do
    let pats = [SourceMarkdown, SourceTemplate ".emabook/templates"] <&> id &&& sourcePattern
    FileSystem.mountOnLVar "." pats model $ \(src, fp) action ->
      case src of
        SourceMarkdown -> case action of
          FileSystem.Update ->
            readMarkdown fp
              <&> maybe id (uncurry M.modelInsert)
          FileSystem.Delete ->
            pure $ maybe id M.modelDelete (R.mkMarkdownRouteFromFilePath fp)
        SourceTemplate dir ->
          M.modelSetHeistTemplate <$> T.loadHeistTemplates dir
  where
    readMarkdown :: (MonadIO m, MonadLogger m) => FilePath -> m (Maybe (MarkdownRoute, (M.Meta, Pandoc)))
    readMarkdown fp =
      runMaybeT $ do
        r :: MarkdownRoute <- MaybeT $ pure $ R.mkMarkdownRouteFromFilePath fp
        logD $ "Reading " <> toText fp
        !s <- readFileText fp
        case parseMarkdown fp s of
          Left (BadMarkdown -> err) -> do
            throw err
          Right (mMeta, doc) ->
            pure (r, (fromMaybe def mMeta, doc))
    parseMarkdown =
      Markdown.parseMarkdownWithFrontMatter @M.Meta $
        Markdown.wikilinkSpec <> Markdown.fullMarkdownSpec

newtype BadMarkdown = BadMarkdown Text
  deriving (Show, Exception)

-- ------------------------
-- Our site rendering
-- ------------------------

render :: Ema.CLI.Action -> Model -> MarkdownRoute -> LByteString
render _ model r = do
  let mDoc = M.modelLookup r model
  -- TODO: Look for "${r}" template, and then fallback to _default
  flip (T.renderHeistTemplate "_default") (M.modelHeistTemplate model) $ do
    -- Common stuff
    "theme" ## HI.textSplice "yellow"
    -- Nav stuff
    "ema:route-tree"
      ## ( let tree = PathTree.treeDeleteChild "index" $ M.modelNav model
            in Splices.treeSplice tree r R.MarkdownRoute $ H.toHtml . flip M.routeTitle model
         )
    "ema:breadcrumbs"
      ## Splices.listSplice (init $ R.markdownRouteInits r) "crumb"
      $ \crumb ->
        MapSyntax.mapV HI.textSplice $ do
          "crumb:url" ## Ema.routeUrl crumb
          "crumb:title" ## M.routeTitle crumb model
    -- Note stuff
    "ema:note:title"
      ## HI.textSplice
      $ if r == R.indexMarkdownRoute
        then "emabook"
        else M.routeTitle r model
    "ema:note:pandoc"
      ## Splices.pandocSplice
      $ case mDoc of
        Nothing ->
          -- This route doesn't correspond to any Markdown file on disk. Could be one of the reasons,
          -- 1. Refers to a folder route (and no ${folder}.md exists)
          -- 2. A broken wiki-links
          -- In both cases, we take the lenient approach, and display an empty page (but with title).
          -- TODO: Display folder children if this is a folder note. It is hinted to in the sidebar too.
          Pandoc mempty $ one $ B.Plain $ one $ B.Str "No Markdown file for this route"
        Just doc ->
          sanitizeMarkdown model doc

sanitizeMarkdown :: Model -> Pandoc -> Pandoc
sanitizeMarkdown model doc =
  doc
    & PandocUtil.withoutH1 -- Eliminate H1, because we are handling it separately.
    & rewriteWikiLinks
  where
    rewriteWikiLinks =
      PandocUtil.rewriteLinks $ \url -> fromMaybe url $ do
        guard $ not $ "://" `T.isInfixOf` url -- Only handle relative URLs
        Ema.routeUrl
          <$> (mkMarkdownRouteFromUrl url <|> mkMarkdownRouteFromWikiLink url)
    mkMarkdownRouteFromUrl :: Text -> Maybe MarkdownRoute
    mkMarkdownRouteFromUrl url = do
      guard $ ".md" `T.isSuffixOf` url
      R.mkMarkdownRouteFromFilePath $ toString url
    mkMarkdownRouteFromWikiLink :: Text -> Maybe MarkdownRoute
    mkMarkdownRouteFromWikiLink s = do
      guard $ not $ "/" `T.isSuffixOf` s
      M.modelLookupFileName s model
