{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Exception (throw)
import Control.Lens.Operators ((.~), (^.))
import Control.Monad.Logger
import qualified Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NE
import Data.Map.Syntax ((##))
import qualified Data.Map.Syntax as MapSyntax
import qualified Data.Yaml as Yaml
import Ema (Ema)
import qualified Ema
import qualified Ema.CLI
import qualified Ema.Helper.FileSystem as FileSystem
import qualified Ema.Helper.Markdown as Markdown
import qualified Ema.Helper.PathTree as PathTree
import Emabook.Model (Model)
import qualified Emabook.Model as M
import qualified Emabook.Model.Note as MN
import Emabook.Route (MarkdownRoute)
import qualified Emabook.Route as R
import qualified Emabook.Template as T
import qualified Emabook.Template.Splices.List as Splices
import qualified Emabook.Template.Splices.Pandoc as Splices
import qualified Emabook.Template.Splices.Tree as Splices
import qualified Heist.Interpreted as HI
import qualified Heist.Splices as Heist
import qualified Heist.Splices.Apply as HA
import qualified Heist.Splices.Bind as HB
import qualified Heist.Splices.Json as HJ
import System.FilePath ((</>))
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
  | SourceData
  | SourceTemplate FilePath
  deriving (Eq, Show)

sourcePattern :: Source -> FilePath
sourcePattern = \case
  SourceMarkdown -> "**/*.md"
  SourceData -> "**/*.yaml"
  SourceTemplate dir -> dir </> "**/*.tpl"

instance Ema Model MarkdownRoute where
  encodeRoute = R.encodeRoute
  decodeRoute = R.decodeRoute
  staticRoutes = M.staticRoutes
  staticAssets _ =
    ["favicon.jpeg", "favicon.svg", "static"]

main :: IO ()
main =
  Ema.runEma render $ \model -> do
    let pats =
          (id &&& sourcePattern)
            <$> [ SourceMarkdown,
                  SourceData,
                  SourceTemplate ".emabook/templates"
                ]
    FileSystem.mountOnLVar "." pats model $ \(src, fp) action ->
      case src of
        SourceMarkdown -> case action of
          FileSystem.Update ->
            fmap (fromMaybe id) . runMaybeT $ do
              r :: MarkdownRoute <- MaybeT $ pure $ R.mkRouteFromFilePath @R.Md fp
              logD $ "Reading note " <> toText fp
              !s <- readFileText fp
              (mMeta, doc) <- either (throw . BadInput) pure $ parseMarkdown fp s
              pure $ M.modelInsert r (fromMaybe Aeson.Null mMeta, doc)
          FileSystem.Delete ->
            pure $ maybe id M.modelDelete (R.mkRouteFromFilePath @R.Md fp)
        SourceData -> case action of
          FileSystem.Update -> do
            fmap (fromMaybe id) . runMaybeT $ do
              r :: R.Route R.Yaml <- MaybeT $ pure $ R.mkRouteFromFilePath @R.Yaml fp
              logD $ "Reading data " <> toText fp
              !s <- readFileBS fp
              sdata <-
                either (throw . BadInput . show) pure $
                  Yaml.decodeEither' s
              pure $ M.modelInsertData r sdata
          FileSystem.Delete ->
            pure $ maybe id M.modelDeleteData (R.mkRouteFromFilePath @R.Yaml fp)
        SourceTemplate dir ->
          (M.modelHeistTemplate .~) <$> T.loadHeistTemplates dir
  where
    parseMarkdown =
      Markdown.parseMarkdownWithFrontMatter @Aeson.Value $
        Markdown.wikilinkSpec <> Markdown.fullMarkdownSpec

newtype BadInput = BadInput Text
  deriving (Show, Exception)

render :: Ema.CLI.Action -> Model -> MarkdownRoute -> LByteString
render _ model r = do
  let mNote = M.modelLookup r model
  -- TODO: Look for "${r}" template, and then fallback to _default
  flip (T.renderHeistTemplate "_default") (model ^. M.modelHeistTemplate) $ do
    "bind" ## HB.bindImpl
    "apply" ## HA.applyImpl
    -- Binding to <html> so they remain in scope throughout.
    "html"
      ## HJ.bindJson (M.modelComputeMeta r model)
    -- Nav stuff
    "ema:route-tree"
      ## ( let tree = PathTree.treeDeleteChild "index" $ model ^. M.modelNav
               getOrder tr =
                 (M.lookupNoteMeta @Int 0 "order" tr model, maybe (R.routeFileBase tr) MN.noteTitle $ M.modelLookup tr model)
            in Splices.treeSplice (getOrder . R.Route) tree $ \(R.Route -> nodeRoute) -> do
                 "node:text" ## HI.textSplice $ M.modelLookupTitle nodeRoute model
                 "node:url" ## HI.textSplice $ Ema.routeUrl nodeRoute
                 let isActiveNode = nodeRoute == r
                     isActiveTree =
                       toList (R.unRoute nodeRoute) `NE.isPrefixOf` R.unRoute r
                 "node:active" ## Heist.ifElseISplice isActiveNode
                 "tree:active" ## Heist.ifElseISplice isActiveTree
         )
    "ema:breadcrumbs"
      ## Splices.listSplice (init $ R.routeInits r) "each-crumb"
      $ \crumb ->
        MapSyntax.mapV HI.textSplice $ do
          "crumb:url" ## Ema.routeUrl crumb
          "crumb:title" ## M.modelLookupTitle crumb model
    -- Note stuff
    "ema:note:title"
      ## HI.textSplice
      $ M.modelLookupTitle r model
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
          M.sanitizeMarkdown model r $ note ^. MN.noteDoc
