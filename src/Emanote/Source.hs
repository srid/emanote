{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.Source
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
import Emanote.Logging (logD)
import Emanote.Model (Model)
import qualified Emanote.Model as M
import qualified Emanote.Model.Note as N
import qualified Emanote.Model.SData as SD
import qualified Emanote.Route as R
import qualified Emanote.Route.Ext as Ext
import Emanote.Route.Linkable (liftLinkableLMLRoute)
import Emanote.Source.Loc (Loc, locLayers, locResolve)
import qualified Emanote.Source.Mount as Mount
import Emanote.Source.Pattern (filePatterns, ignorePatterns)
import Emanote.Source.Util
  ( BadInput (BadInput),
    chainM,
  )
import qualified Heist.Extra.TemplateState as T
import UnliftIO.IO (hFlush)

-- | Like `transformAction` but operates on multiple source types at a time
transformActions :: (MonadIO m, MonadLogger m) => Mount.Change Loc Ext.FileType -> m (Model -> Model)
transformActions ch = do
  chainM (Map.toList ch) (uncurry transformAction)
    <* hFlush stdout

-- | Transform a filesystem action (on a source) to model update
transformAction ::
  (MonadIO m, MonadLogger m) =>
  Ext.FileType ->
  Map FilePath (Mount.FileAction (NonEmpty (Loc, FilePath))) ->
  m (Model -> Model)
transformAction src fps = do
  chainM (Map.toList fps) $ \(fp, action) -> case src of
    Ext.LMLType Ext.Md ->
      case fmap liftLinkableLMLRoute . R.mkRouteFromFilePath @('Ext.LMLType 'Ext.Md) $ fp of
        Nothing ->
          pure id
        Just r -> case action of
          Mount.Update overlays -> do
            let fpAbs = locResolve $ head overlays
            logD $ "Reading note: " <> toText fpAbs
            fmap M.modelInsertNote $ either (throw . BadInput) pure =<< N.parseNote r fpAbs
          Mount.Delete ->
            pure $ M.modelDeleteNote r
    Ext.Yaml ->
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
    Ext.HeistTpl ->
      case action of
        Mount.Update overlays -> do
          let fpAbs = locResolve $ head overlays
          fmap (M.modelHeistTemplate %~) $ do
            logD $ "Reading template: " <> toText fpAbs
            s <- readFileBS fpAbs
            pure $ T.addTemplateFile fpAbs fp s
        Mount.Delete -> do
          pure $ M.modelHeistTemplate %~ T.removeTemplateFile fp
    Ext.AnyExt -> do
      case R.mkRouteFromFilePath fp of
        Nothing ->
          pure id
        Just r -> case action of
          Mount.Update overlays -> do
            let fpAbs = locResolve $ head overlays
            logD $ "Adding file: " <> toText fpAbs <> " " <> show r
            pure $ M.modelInsertStaticFile r fpAbs
          Mount.Delete -> do
            pure $ M.modelDeleteStaticFile r
    Ext.Html -> do
      -- HTML is handled by AnyExt above, beause we are not passing this to `unionMount`
      pure id
