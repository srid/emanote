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
  deriving (Eq, Show)

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
          SourceTemplate ".emabook/templates"
        ]

-- | Transform a filesystem action (on a source) to model update
transformAction :: (MonadIO m, MonadLogger m) => Source -> FilePath -> FileSystem.FileAction -> m (Model -> Model)
transformAction src fp action =
  case src of
    SourceMarkdown -> case action of
      FileSystem.Update ->
        fmap (fromMaybe id) . runMaybeT $ do
          r :: MarkdownRoute <- MaybeT $ pure $ R.mkRouteFromFilePath @Ext.Md fp
          logD $ "Reading note " <> toText fp
          !s <- readFileText fp
          (mMeta, doc) <- either (throw . BadInput) pure $ parseMarkdown fp s
          pure $ M.modelInsert r (fromMaybe Aeson.Null mMeta, doc)
      FileSystem.Delete ->
        pure $ maybe id M.modelDelete (R.mkRouteFromFilePath @Ext.Md fp)
    SourceData -> case action of
      FileSystem.Update -> do
        fmap (fromMaybe id) . runMaybeT $ do
          r :: R.Route Ext.Yaml <- MaybeT $ pure $ R.mkRouteFromFilePath @Ext.Yaml fp
          logD $ "Reading data " <> toText fp
          !s <- readFileBS fp
          sdata <-
            either (throw . BadInput . show) pure $
              Yaml.decodeEither' s
          pure $ M.modelInsertData r sdata
      FileSystem.Delete ->
        pure $ maybe id M.modelDeleteData (R.mkRouteFromFilePath @Ext.Yaml fp)
    SourceTemplate dir ->
      (M.modelHeistTemplate .~) <$> T.loadHeistTemplates dir
  where
    parseMarkdown =
      Markdown.parseMarkdownWithFrontMatter @Aeson.Value $
        Markdown.wikilinkSpec <> Markdown.fullMarkdownSpec

newtype BadInput = BadInput Text
  deriving (Show, Exception)
