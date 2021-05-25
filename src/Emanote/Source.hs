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
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Ema.Helper.Markdown as Markdown
import Emanote.Logging
import Emanote.Model (Model)
import qualified Emanote.Model as M
import qualified Emanote.Model.Meta as Meta
import qualified Emanote.Route as R
import Emanote.Route.Ext (FileType (LMLType), LML (Md))
import qualified Emanote.Route.Ext as Ext
import qualified Emanote.Source.Mount as Mount
import Emanote.Source.Util
  ( BadInput (BadInput),
    chainM,
    parseSData,
  )
import qualified Heist.Extra.TemplateState as T
import System.FilePath ((</>))
import System.FilePattern (FilePattern)

data Loc
  = -- | The location of the emanote's default files directory containing
    -- templates, data, etc.
    LocEmanoteDefault FilePath
  | -- | This always refers to current working directory
    LocUser
  deriving (Eq, Ord, Show)

locResolve :: (Loc, FilePath) -> FilePath
locResolve (loc, fp) = case loc of
  LocUser -> fp -- CWD
  LocEmanoteDefault base -> base </> fp

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
transformActions :: (MonadIO m, MonadLogger m) => Mount.Change Loc Source -> Mount.FileAction () -> m (Model -> Model)
transformActions ch action = do
  print ch
  chainM (Map.toList ch) $ \(src, fps) ->
    transformAction src fps

-- | Transform a filesystem action (on a source) to model update
transformAction ::
  (MonadIO m, MonadLogger m) =>
  Source ->
  Map FilePath (Mount.FileAction (NonEmpty (Loc, FilePath))) ->
  m (Model -> Model)
transformAction src fps = do
  case src of
    SourceLML Ext.Md ->
      chainM (Map.toList fps) $ \(fp, action) ->
        case action of
          Mount.Update overlays ->
            fmap (fromMaybe id) . runMaybeT $ do
              print overlays
              let fpAbs = locResolve $ head overlays
              r :: R.Route ('LMLType 'Md) <- MaybeT $ pure $ R.mkRouteFromFilePath @('Ext.LMLType 'Ext.Md) fp
              -- TODO: Log in batches, to avoid slowing things down when using large notebooks
              logD $ "Reading note: " <> toText fpAbs
              !s <- readFileText fpAbs
              (mMeta, doc) <- either (throw . BadInput) pure $ parseMarkdown fpAbs s
              pure $ M.modelInsertMarkdown r (fromMaybe Aeson.Null mMeta, doc)
          Mount.Delete ->
            pure $ maybe id M.modelDeleteMarkdown (R.mkRouteFromFilePath @('Ext.LMLType 'Ext.Md) fp)
    SourceData ->
      chainM (Map.toList fps) $ \(fp, action) ->
        case action of
          Mount.Update overlays ->
            fmap (fromMaybe id) . runMaybeT $ do
              print overlays
              let fpAbs = locResolve $ head overlays
              -- TODO: We want to merge index.yaml from two locations. Not favour the user one!
              r :: R.Route 'Ext.Yaml <- MaybeT $ pure $ R.mkRouteFromFilePath @'Ext.Yaml fp
              logD $ "Reading data: " <> toText fpAbs
              !s <- readFileBS fpAbs
              sdata <- parseSData s
              {- final <- case fst (snd x) of
                Just (LocEmanoteDefault defaultDir) -> do
                  let baseYaml = defaultDir </> snd (snd x)
                  logD $ "Reading default data: " <> toText baseYaml
                  !baseS <- readFileBS baseYaml
                  baseData <- parseSData baseS
                  pure $ Meta.mergeAeson baseData sdata
                _ -> pure sdata -}
              pure $ M.modelInsertData r sdata
          Mount.Delete ->
            pure $ maybe id M.modelDeleteData (R.mkRouteFromFilePath @'Ext.Yaml fp)
    SourceTemplate ->
      chainM (Map.toList fps) $ \(fp, action) ->
        case action of
          Mount.Update overlays -> do
            print overlays
            let fpAbs = locResolve $ head overlays
            fmap (M.modelHeistTemplate %~) $ do
              logD $ "Reading template: " <> toText fpAbs
              s <- readFileBS fpAbs
              -- TODO: Set template name!
              pure $ T.addTemplateFile fpAbs fp s
          Mount.Delete -> do
            -- TODO: Handle *removing* of templates! ... however, don't remove *default* ones.
            -- Removing a default template, should restore it.
            pure id
    SourceStatic -> do
      chainM (Map.toList fps) $ \(fp, action) -> do
        case action of
          Mount.Update overlays -> do
            print overlays
            let fpAbs = locResolve $ head overlays
            pure $ M.modelStaticFiles %~ Set.union (maybe mempty Set.singleton $ R.mkRouteFromFilePath fpAbs)
          Mount.Delete ->
            pure $ M.modelStaticFiles %~ maybe id Set.delete (R.mkRouteFromFilePath fp)
  where
    parseMarkdown =
      Markdown.parseMarkdownWithFrontMatter @Aeson.Value $
        Markdown.wikilinkSpec <> Markdown.fullMarkdownSpec
