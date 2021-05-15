{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception (throw)
import Control.Monad.Logger
import Control.Monad.Writer.Strict (runWriter)
import Data.Default (Default (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Map.Syntax ((##))
import qualified Data.Map.Syntax as MapSyntax
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
import qualified Heist.Splices.Json as HJ
import System.FilePath ((</>))
import qualified Text.Blaze.Html5 as H
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))

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
  | SourceTemplateSettings FilePath
  deriving (Eq, Show)

sourcePattern :: Source -> FilePath
sourcePattern = \case
  SourceMarkdown -> "**/*.md"
  SourceTemplate dir -> dir </> "*.tpl"
  SourceTemplateSettings fp -> fp

main :: IO ()
main =
  Ema.runEma render $ \model -> do
    let pats =
          (id &&& sourcePattern)
            <$> [ SourceMarkdown,
                  SourceTemplate ".emabook/templates",
                  SourceTemplateSettings ".emabook/templates/settings.yml"
                ]
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
        SourceTemplateSettings _ ->
          M.modelUpdateSettings fp <$> readFileText fp
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

render :: Ema.CLI.Action -> Model -> MarkdownRoute -> LByteString
render _ model r = do
  let mNote = M.modelLookup r model
  -- TODO: Look for "${r}" template, and then fallback to _default
  flip (T.renderHeistTemplate "_default") (M.modelHeistTemplate model) $ do
    -- Common stuff from settings.yml
    -- Binding to <html> so they remain in scope throughout.
    "html" ## HJ.bindJson (M.modelSettings model)
    -- Nav stuff
    "ema:route-tree"
      ## ( let tree = PathTree.treeDeleteChild "index" $ M.modelNav model
               getTreeLoc treeR
                 | treeR == R.unMarkdownRoute r = Splices.TreeLoc_Current
                 | toList treeR `NE.isPrefixOf` R.unMarkdownRoute r = Splices.TreeLoc_Ancestor
                 | otherwise = Splices.TreeLoc_Elsewhere
            in Splices.treeSplice tree R.MarkdownRoute getTreeLoc $ H.toHtml . flip M.modelLookupTitle model
         )
    "ema:breadcrumbs"
      ## Splices.listSplice (init $ R.markdownRouteInits r) "crumb"
      $ \crumb ->
        MapSyntax.mapV HI.textSplice $ do
          "crumb:url" ## Ema.routeUrl crumb
          "crumb:title" ## M.modelLookupTitle crumb model
    -- Note stuff
    "ema:note:title"
      ## HI.textSplice
      $ M.modelLookupTitle r model
    "ema:note:tags"
      ## Splices.listSplice (maybe mempty (M.tags . M.noteMeta) mNote) "tag"
      $ \tag ->
        MapSyntax.mapV HI.textSplice $ do
          "tag:name" ## tag
    "ema:note:backlinks"
      ## Splices.listSplice (M.modelLookupBacklinks r model) "backlink"
      $ \(source, ctx) -> do
        let ctxDoc :: Pandoc = Pandoc mempty $ B.Div B.nullAttr <$> toList ctx
        -- TODO: reuse note splice
        "backlink:note:title" ## HI.textSplice (M.modelLookupTitle source model)
        "backlink:note:url" ## HI.textSplice (Ema.routeUrl source)
        "backlink:note:context"
          ## Splices.pandocSplice
          $ ctxDoc
    "ema:note:pandoc"
      ## Splices.pandocSplice
      $ case mNote of
        Nothing ->
          -- This route doesn't correspond to any Markdown file on disk. Could be one of the reasons,
          -- 1. Refers to a folder route (and no ${folder}.md exists)
          -- 2. A broken wiki-links
          -- In both cases, we take the lenient approach, and display an empty page (but with title).
          -- TODO: Display folder children if this is a folder note. It is hinted to in the sidebar too.
          Pandoc mempty $ one $ B.Plain $ one $ B.Str "No Markdown file exists for this route."
        Just note ->
          -- TODO: Need to handle broken links somehow.
          let (doc', Map.fromListWith (<>) . fmap (second $ one @(NonEmpty Text)) -> brokenLinks) =
                runWriter $
                  M.sanitizeMarkdown model r $ M.noteDoc note
           in doc'
