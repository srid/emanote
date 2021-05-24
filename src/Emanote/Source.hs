{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.Source where

import Control.Exception (throw)
import Control.Lens.Operators ((%~))
import Control.Monad.Logger (MonadLogger)
import qualified Data.Aeson as Aeson
import qualified Data.Set as Set
import qualified Ema.Helper.FileSystem as FileSystem
import qualified Ema.Helper.Markdown as Markdown
import Emanote.Logging
import Emanote.Model (Model)
import qualified Emanote.Model as M
import qualified Emanote.Route as R
import Emanote.Route.Ext (FileType (LMLType), LML (Md))
import qualified Emanote.Route.Ext as Ext
import Emanote.Source.Util
  ( BadInput (BadInput),
    chainM,
    parseSData,
  )
import qualified Heist.Extra.TemplateState as T
import System.FilePattern (FilePattern)

-- | Represents the different kinds of file the application will handle.
data Source
  = -- | Markdown file
    SourceLML Ext.LML
  | -- | YAML data file
    SourceData
  | -- | Heist template file
    SourceTemplate
  | -- | The rest are considered static files to copy as-is
    SourceStatic
  deriving (Eq, Ord, Show)

sourcePattern :: Source -> FilePath
sourcePattern = \case
  SourceLML Ext.Md ->
    Ext.withExt @('Ext.LMLType 'Ext.Md) $
      "**/*"
  SourceData ->
    Ext.withExt @'Ext.Yaml $
      "**/*"
  SourceTemplate ->
    Ext.withExt @'Ext.HeistTpl $
      "**/*"
  SourceStatic ->
    "**"

filePatterns :: [(Source, FilePattern)]
filePatterns =
  (id &&& sourcePattern)
    <$> [ SourceLML Ext.Md,
          SourceData,
          SourceTemplate,
          SourceStatic
        ]

ignorePatterns :: [FilePattern]
ignorePatterns =
  [ -- Ignore all top-level dotfile directories (eg: .git, .vscode)
    ".*/**"
  ]

-- | Like `transformAction` but operates on multiple source types at a time
transformActions :: (MonadIO m, MonadLogger m) => [(Source, [FilePath])] -> FileSystem.FileAction -> m (Model -> Model)
transformActions sources action =
  chainM sources $ \(src, fps) ->
    transformAction src fps action

-- | Transform a filesystem action (on a source) to model update
transformAction :: (MonadIO m, MonadLogger m) => Source -> [FilePath] -> FileSystem.FileAction -> m (Model -> Model)
transformAction src fps action =
  case src of
    SourceLML Ext.Md -> case action of
      FileSystem.Update ->
        chainM fps $ \fp ->
          fmap (fromMaybe id) . runMaybeT $ do
            r :: R.Route ('LMLType 'Md) <- MaybeT $ pure $ R.mkRouteFromFilePath @('Ext.LMLType 'Ext.Md) fp
            -- TODO: Log in batches, to avoid slowing things down when using large notebooks
            logD $ "Reading note: " <> toText fp
            !s <- readFileText fp
            (mMeta, doc) <- either (throw . BadInput) pure $ parseMarkdown fp s
            pure $ M.modelInsertMarkdown r (fromMaybe Aeson.Null mMeta, doc)
      FileSystem.Delete ->
        chainM fps $ \fp ->
          pure $ maybe id M.modelDeleteMarkdown (R.mkRouteFromFilePath @('Ext.LMLType 'Ext.Md) fp)
    SourceData -> case action of
      FileSystem.Update -> do
        chainM fps $ \fp ->
          fmap (fromMaybe id) . runMaybeT $ do
            r :: R.Route 'Ext.Yaml <- MaybeT $ pure $ R.mkRouteFromFilePath @'Ext.Yaml fp
            logD $ "Reading data: " <> toText fp
            !s <- readFileBS fp
            sdata <- parseSData s
            pure $ M.modelInsertData r sdata
      FileSystem.Delete ->
        chainM fps $ \fp ->
          pure $ maybe id M.modelDeleteData (R.mkRouteFromFilePath @'Ext.Yaml fp)
    SourceTemplate -> do
      print fps
      -- TODO: Handle *removing* of templates! ... however, don't remove *default* ones.
      fmap (M.modelHeistTemplate %~) $
        chainM fps $ \fp -> do
          logD $ "Reading template: " <> toText fp
          s <- readFileBS fp
          pure $ T.addTemplateFile fp s
    SourceStatic -> do
      let setAction = case action of
            FileSystem.Update -> Set.union
            FileSystem.Delete -> flip Set.difference
      pure $ M.modelStaticFiles %~ setAction (Set.fromList $ mapMaybe R.mkRouteFromFilePath fps)
  where
    parseMarkdown =
      Markdown.parseMarkdownWithFrontMatter @Aeson.Value $
        Markdown.wikilinkSpec <> Markdown.fullMarkdownSpec
