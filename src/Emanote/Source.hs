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
import qualified Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import qualified Data.Yaml as Yaml
import qualified Ema.Helper.Markdown as Markdown
import Emanote.Logging (logD)
import Emanote.Model (Model)
import qualified Emanote.Model as M
import qualified Emanote.Model.Meta as Meta
import qualified Emanote.Route as R
import qualified Emanote.Route.Ext as Ext
import Emanote.Route.SomeRoute (liftSomeLMLRoute)
import Emanote.Source.Loc (Loc, locLayers, locResolve)
import qualified Emanote.Source.Mount as Mount
import Emanote.Source.Pattern (filePatterns, ignorePatterns)
import Emanote.Source.Util
  ( BadInput (BadInput),
    chainM,
  )
import qualified Heist.Extra.TemplateState as T

-- | Like `transformAction` but operates on multiple source types at a time
transformActions :: (MonadIO m, MonadLogger m) => Mount.Change Loc Ext.FileType -> m (Model -> Model)
transformActions ch = do
  chainM (Map.toList ch) $ uncurry transformAction

-- | Transform a filesystem action (on a source) to model update
transformAction ::
  (MonadIO m, MonadLogger m) =>
  Ext.FileType ->
  Map FilePath (Mount.FileAction (NonEmpty (Loc, FilePath))) ->
  m (Model -> Model)
transformAction src fps = do
  chainM (Map.toList fps) $ \(fp, action) -> do
    let mkMdRoute = fmap liftSomeLMLRoute . R.mkRouteFromFilePath @('Ext.LMLType 'Ext.Md)
    case src of
      Ext.LMLType Ext.Md ->
        case action of
          Mount.Update overlays ->
            fmap (fromMaybe id) . runMaybeT $ do
              let fpAbs = locResolve $ head overlays
              r <- MaybeT $ pure $ mkMdRoute fp
              -- TODO: Log in batches, to avoid slowing things down when using large notebooks
              logD $ "Reading note: " <> toText fpAbs
              !s <- readFileText fpAbs
              (mMeta, doc) <- either (throw . BadInput) pure $ parseMarkdown fpAbs s
              pure $ M.modelInsertNote r (fromMaybe Aeson.Null mMeta, doc)
          Mount.Delete ->
            pure $ maybe id M.modelDeleteNote (mkMdRoute fp)
      Ext.Yaml ->
        case action of
          Mount.Update overlays ->
            fmap (fromMaybe id) . runMaybeT $ do
              r :: R.Route 'Ext.Yaml <- MaybeT $ pure $ R.mkRouteFromFilePath @'Ext.Yaml fp
              fmap (M.modelInsertData r . Meta.mergeAesons . NEL.reverse) $
                forM overlays $ \overlay -> do
                  let fpAbs = locResolve overlay
                  logD $ "Reading data: " <> toText fpAbs
                  !s <- readFileBS fpAbs
                  parseSData s
          Mount.Delete ->
            pure $ maybe id M.modelDeleteData (R.mkRouteFromFilePath @'Ext.Yaml fp)
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
        fmap (fromMaybe id) . runMaybeT $ do
          r <- MaybeT $ pure $ R.mkRouteFromFilePath fp
          case action of
            Mount.Update overlays -> do
              let fpAbs = locResolve $ head overlays
              logD $ "Adding file: " <> toText fpAbs <> " " <> show r
              pure $ M.modelStaticFiles %~ Map.insert r fpAbs
            Mount.Delete -> do
              pure $ M.modelStaticFiles %~ Map.delete r
      Ext.Html -> do
        -- HTML is handled by AnyExt above, beause we are not passing this to `unionMount`
        pure id
  where
    parseMarkdown =
      Markdown.parseMarkdownWithFrontMatter @Aeson.Value $
        Markdown.wikilinkSpec <> Markdown.fullMarkdownSpec
    parseSData :: (Applicative f, Yaml.FromJSON a) => ByteString -> f a
    parseSData s =
      either (throw . BadInput . show) pure $
        Yaml.decodeEither' s
