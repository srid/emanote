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
  )
where

import Control.Exception (throw)
import Control.Lens.Operators ((%~))
import Control.Monad.Logger (MonadLogger)
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import Data.Time (getCurrentTime)
import qualified Ema.Helper.FileSystem as EmaFS
import qualified Emanote.Model as M
import qualified Emanote.Model.Note as N
import qualified Emanote.Model.SData as SD
import Emanote.Model.Type (Model)
import Emanote.Prelude
  ( BadInput (BadInput),
    chainM,
    logD,
  )
import Emanote.Route (liftLMLRoute)
import qualified Emanote.Route as R
import Emanote.Source.Loc (Loc, locResolve)
import Emanote.Source.Pattern (filePatterns, ignorePatterns)
import qualified Heist.Extra.TemplateState as T
import UnliftIO (BufferMode (..), hSetBuffering)
import UnliftIO.Directory (doesDirectoryExist)
import UnliftIO.IO (hFlush)

-- | Like `transformAction` but operates on multiple source types at a time
transformActions :: (MonadIO m, MonadLogger m) => EmaFS.Change Loc R.FileType -> m (Model -> Model)
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
  Map FilePath (EmaFS.FileAction (NonEmpty (Loc, FilePath))) ->
  m (Model -> Model)
transformAction src fps = do
  flip chainM (Map.toList fps) $ \(fp, action) -> case src of
    R.LMLType R.Md ->
      case fmap liftLMLRoute . R.mkRouteFromFilePath @('R.LMLType 'R.Md) $ fp of
        Nothing ->
          pure id
        Just r -> case action of
          EmaFS.Update overlays -> do
            let fpAbs = locResolve $ head overlays
            logD $ "Reading note: " <> toText fpAbs
            fmap M.modelInsertNote $ either (throw . BadInput) pure =<< N.parseNote r fpAbs
          EmaFS.Delete ->
            pure $ M.modelDeleteNote r
    R.Yaml ->
      case R.mkRouteFromFilePath fp of
        Nothing ->
          pure id
        Just r -> case action of
          EmaFS.Update overlays -> do
            yamlContents <- forM (NEL.reverse overlays) $ \overlay -> do
              let fpAbs = locResolve overlay
              logD $ "Reading data: " <> toText fpAbs
              readFileBS fpAbs
            sData <-
              either (throw . BadInput) pure $
                SD.parseSDataCascading r yamlContents
            pure $ M.modelInsertData sData
          EmaFS.Delete ->
            pure $ M.modelDeleteData r
    R.HeistTpl ->
      case action of
        EmaFS.Update overlays -> do
          let fpAbs = locResolve $ head overlays
          fmap (M.modelHeistTemplate %~) $ do
            logD $ "Reading template: " <> toText fpAbs
            s <- readFileBS fpAbs
            logD $ "Read " <> show (BS.length s) <> " bytes of template"
            pure $ T.addTemplateFile fpAbs fp s
        EmaFS.Delete -> do
          pure $ M.modelHeistTemplate %~ T.removeTemplateFile fp
    R.AnyExt -> do
      case R.mkRouteFromFilePath fp of
        Nothing ->
          pure id
        Just r -> case action of
          EmaFS.Update overlays -> do
            let fpAbs = locResolve $ head overlays
            doesDirectoryExist fpAbs >>= \case
              True ->
                -- A directory got added; this is not a static 'file'
                pure id
              False -> do
                logD $ "Adding file: " <> toText fpAbs <> " " <> show r
                t <- liftIO getCurrentTime
                pure $ M.modelInsertStaticFile t r fpAbs
          EmaFS.Delete -> do
            pure $ M.modelDeleteStaticFile r
    R.Html -> do
      -- HTML is handled by AnyExt above, beause we are not passing this to `unionMount`
      pure id
    R.Folder -> do
      -- Unused! But maybe we should ... TODO:
      pure id
