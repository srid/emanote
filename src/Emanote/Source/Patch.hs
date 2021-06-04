{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | Patch model state depending on file change event.
module Emanote.Source.Patch
  ( transformActions,
    filePatterns,
    ignorePatterns,
    locLayers,
  )
where

import Control.Exception (throw)
import Control.Lens.Operators ((%~))
import Control.Monad.Logger (MonadLogger)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import Data.Time (getCurrentTime)
import Emanote.Model (Model)
import qualified Emanote.Model as M
import qualified Emanote.Model.Note as N
import qualified Emanote.Model.SData as SD
import Emanote.Prelude
  ( BadInput (BadInput),
    chainM,
    logD,
  )
import Emanote.Route (liftLinkableLMLRoute)
import qualified Emanote.Route as R
import Emanote.Source.Loc (Loc, locLayers, locResolve)
import qualified Emanote.Source.Mount as Mount
import Emanote.Source.Pattern (filePatterns, ignorePatterns)
import qualified Heist.Extra.TemplateState as T
import UnliftIO (BufferMode (..), hSetBuffering)
import UnliftIO.IO (hFlush)

-- | Like `transformAction` but operates on multiple source types at a time
transformActions :: (MonadIO m, MonadLogger m) => Mount.Change Loc R.FileType -> m (Model -> Model)
transformActions ch = do
  withBlockBuffering $
    uncurry transformAction `chainM` Map.toList ch
  where
    -- Temporarily use block buffering before calling an IO action that is
    -- known ahead to log rapidly, so as to not hamper serial processing speed.
    withBlockBuffering f =
      hSetBuffering stdout (BlockBuffering Nothing)
        *> f
        <* (hSetBuffering stdout LineBuffering >> hFlush stdout)

-- | Transform a filesystem action (on a source) to model update
transformAction ::
  (MonadIO m, MonadLogger m) =>
  R.FileType ->
  Map FilePath (Mount.FileAction (NonEmpty (Loc, FilePath))) ->
  m (Model -> Model)
transformAction src fps = do
  flip chainM (Map.toList fps) $ \(fp, action) -> case src of
    R.LMLType R.Md ->
      case fmap liftLinkableLMLRoute . R.mkRouteFromFilePath @('R.LMLType 'R.Md) $ fp of
        Nothing ->
          pure id
        Just r -> case action of
          Mount.Update overlays -> do
            let fpAbs = locResolve $ head overlays
            logD $ "Reading note: " <> toText fpAbs
            fmap M.modelInsertNote $ either (throw . BadInput) pure =<< N.parseNote r fpAbs
          Mount.Delete ->
            pure $ M.modelDeleteNote r
    R.Yaml ->
      case R.mkRouteFromFilePath fp of
        Nothing ->
          pure id
        Just r -> case action of
          Mount.Update overlays -> do
            yamlContents <- forM (NEL.reverse overlays) $ \overlay -> do
              let fpAbs = locResolve overlay
              logD $ "Reading data: " <> toText fpAbs
              readFileBS fpAbs
            sData <-
              either (throw . BadInput) pure $
                SD.parseSDataCascading r yamlContents
            pure $ M.modelInsertData sData
          Mount.Delete ->
            pure $ M.modelDeleteData r
    R.HeistTpl ->
      case action of
        Mount.Update overlays -> do
          let fpAbs = locResolve $ head overlays
          fmap (M.modelHeistTemplate %~) $ do
            logD $ "Reading template: " <> toText fpAbs
            s <- readFileBS fpAbs
            pure $ T.addTemplateFile fpAbs fp s
        Mount.Delete -> do
          pure $ M.modelHeistTemplate %~ T.removeTemplateFile fp
    R.AnyExt -> do
      case R.mkRouteFromFilePath fp of
        Nothing ->
          pure id
        Just r -> case action of
          Mount.Update overlays -> do
            let fpAbs = locResolve $ head overlays
            logD $ "Adding file: " <> toText fpAbs <> " " <> show r
            t <- liftIO getCurrentTime
            pure $ M.modelInsertStaticFile t r fpAbs
          Mount.Delete -> do
            pure $ M.modelDeleteStaticFile r
    R.Html -> do
      -- HTML is handled by AnyExt above, beause we are not passing this to `unionMount`
      pure id
    R.Folder -> do
      -- Unused! But maybe we should ... TODO:
      pure id
