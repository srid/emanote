{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Emabook.Source where

import Control.Exception (throw)
import Control.Lens.Operators ((.~))
import Control.Monad.Logger
import qualified Data.Aeson as Aeson
import qualified Data.Yaml as Yaml
import qualified Ema.Helper.FileSystem as FileSystem
import qualified Ema.Helper.Markdown as Markdown
import Emabook.Model (Model)
import qualified Emabook.Model as M
import Emabook.Route (MarkdownRoute)
import qualified Emabook.Route as R
import qualified Emabook.Route.Ext as Ext
import qualified Heist.Extra.TemplateState as T
import qualified Paths_emabook
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))

log :: MonadLogger m => Text -> m ()
log = logInfoNS "emabook"

logD :: MonadLogger m => Text -> m ()
logD = logDebugNS "emabook"

logE :: MonadLogger m => Text -> m ()
logE = logErrorNS "emabook"

-- | Represents the different kinds of file the application will handle.
data Source
  = -- | Markdown file
    SourceMarkdown
  | -- | YAML data file
    SourceData
  | -- | Heist template file
    SourceTemplate FilePath
  deriving (Eq, Ord, Show)

sourcePattern :: Source -> FilePath
sourcePattern = \case
  SourceMarkdown -> "**/*.md"
  SourceData -> "**/*.yaml"
  SourceTemplate dir -> dir </> "**/*.tpl"

filePatterns :: [(Source, FilePath)]
filePatterns =
  (id &&& sourcePattern)
    <$> [ SourceMarkdown,
          SourceData,
          SourceTemplate "templates"
        ]

defaultTemplateState :: (MonadIO m, MonadLogger m) => m T.TemplateState
defaultTemplateState = do
  defaultFiles <- liftIO Paths_emabook.getDataDir
  let tmplDir = defaultFiles </> "templates"
  log $ "Loading default templates from " <> toText tmplDir
  T.loadHeistTemplates tmplDir

defaultData :: (MonadIO m, MonadLogger m) => m Aeson.Value
defaultData = do
  defaultFiles <- liftIO Paths_emabook.getDataDir
  let indexYaml = defaultFiles </> "index.yaml"
  log $ "Loading index.yaml from " <> toText indexYaml
  parseSData =<< readFileBS indexYaml

-- | Like `transformAction` but operates on multiple source types at a time
transformActions :: (MonadIO m, MonadLogger m) => [(Source, [FilePath])] -> FileSystem.FileAction -> m (Model -> Model)
transformActions sources action =
  chainM sources $ \(src, fps) ->
    transformAction src fps action

-- | Transform a filesystem action (on a source) to model update
transformAction :: (MonadIO m, MonadLogger m) => Source -> [FilePath] -> FileSystem.FileAction -> m (Model -> Model)
transformAction src fps action =
  case src of
    SourceMarkdown -> case action of
      FileSystem.Update ->
        chainM fps $ \fp ->
          fmap (fromMaybe id) . runMaybeT $ do
            r :: MarkdownRoute <- MaybeT $ pure $ R.mkRouteFromFilePath @Ext.Md fp
            logD $ "Reading note " <> toText fp
            !s <- readFileText fp
            (mMeta, doc) <- either (throw . BadInput) pure $ parseMarkdown fp s
            pure $ M.modelInsertMarkdown r (fromMaybe Aeson.Null mMeta, doc)
      FileSystem.Delete ->
        chainM fps $ \fp ->
          pure $ maybe id M.modelDeleteMarkdown (R.mkRouteFromFilePath @Ext.Md fp)
    SourceData -> case action of
      FileSystem.Update -> do
        chainM fps $ \fp ->
          fmap (fromMaybe id) . runMaybeT $ do
            r :: R.Route Ext.Yaml <- MaybeT $ pure $ R.mkRouteFromFilePath @Ext.Yaml fp
            logD $ "Reading data " <> toText fp
            !s <- readFileBS fp
            sdata <- parseSData s
            pure $ M.modelInsertData r sdata
      FileSystem.Delete ->
        chainM fps $ \fp ->
          pure $ maybe id M.modelDeleteData (R.mkRouteFromFilePath @Ext.Yaml fp)
    SourceTemplate dir -> do
      liftIO (doesDirectoryExist dir) >>= \case
        True -> do
          log "Reloading user templates"
          (M.modelHeistTemplate .~) <$> T.loadHeistTemplates dir
        False -> do
          -- Revert to default templates (the user has deleted theirs)
          (M.modelHeistTemplate .~) <$> defaultTemplateState
  where
    parseMarkdown =
      Markdown.parseMarkdownWithFrontMatter @Aeson.Value $
        Markdown.wikilinkSpec <> Markdown.fullMarkdownSpec

parseSData :: (Applicative f, Yaml.FromJSON a) => ByteString -> f a
parseSData s =
  either (throw . BadInput . show) pure $
    Yaml.decodeEither' s

-- | Apply the list of actions in the given order to an initial argument.
--
-- chain [f1, f2, ...] x = ... (f2 (f1 x))
chain :: [a -> a] -> a -> a
chain = flip (foldl' $ flip ($))

-- | Monadic version of `chain`
chainM :: Monad m => [b] -> (b -> m (a -> a)) -> m (a -> a)
chainM xs =
  fmap chain . forM xs

newtype BadInput = BadInput Text
  deriving (Show, Exception)
