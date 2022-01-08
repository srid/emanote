{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

-- | Pattch model state depending on file change event.
--
-- See `mapFsChange` for more details.
module Emanote.Source.Patch
  ( patchModel,
    filePatterns,
    ignorePatterns,
  )
where

import Control.Exception (throw)
import Control.Lens.Operators ((%~))
import Control.Monad.Logger (MonadLogger)
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NEL
import Data.Time (getCurrentTime)
import qualified Ema.Helper.FileSystem as EmaFS
import qualified Emanote.Model as M
import qualified Emanote.Model.Note as N
import qualified Emanote.Model.SData as SD
import Emanote.Model.Type (Model)
import Emanote.Prelude
  ( BadInput (BadInput),
    log,
    logD,
  )
import Emanote.Route (liftLMLRoute)
import qualified Emanote.Route as R
import Emanote.Source.Loc (Loc, locResolve)
import Emanote.Source.Pattern (filePatterns, ignorePatterns)
import qualified Heist.Extra.TemplateState as T
import Relude
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Directory (doesDirectoryExist)

-- | Map a filesystem change to the corresponding model change.
patchModel ::
  (MonadIO m, MonadLogger m) =>
  -- | Type of the file being changed
  R.FileType R.SourceExt ->
  -- | Path to the file being changed
  FilePath ->
  -- | Specific change to the file, along with its paths from other "layers"
  EmaFS.FileAction (NonEmpty (Loc, FilePath)) ->
  m (Model -> Model)
patchModel fpType fp action =
  case fpType of
    R.LMLType R.Md ->
      case fmap liftLMLRoute . R.mkRouteFromFilePath @R.SourceExt @('R.LMLType 'R.Md) $ fp of
        Nothing ->
          pure id
        Just r -> case action of
          EmaFS.Refresh refreshAction overlays -> do
            let fpAbs = locResolve $ head overlays
            s <- readRefreshedFile refreshAction fpAbs
            case N.parseNote r fpAbs (decodeUtf8 s) of
              Left e ->
                throw $ BadInput e
              Right note ->
                pure $ M.modelInsertNote note
          EmaFS.Delete -> do
            log $ "Removing note: " <> toText fp
            pure $ M.modelDeleteNote r
    R.Yaml ->
      case R.mkRouteFromFilePath fp of
        Nothing ->
          pure id
        Just r -> case action of
          EmaFS.Refresh refreshAction overlays -> do
            yamlContents <- forM (NEL.reverse overlays) $ \overlay -> do
              let fpAbs = locResolve overlay
              readRefreshedFile refreshAction fpAbs
            sData <-
              either (throw . BadInput) pure $
                SD.parseSDataCascading r yamlContents
            pure $ M.modelInsertData sData
          EmaFS.Delete -> do
            log $ "Removing data: " <> toText fp
            pure $ M.modelDeleteData r
    R.HeistTpl ->
      case action of
        EmaFS.Refresh refreshAction overlays -> do
          let fpAbs = locResolve $ head overlays
              -- Once we start loading HTML templates, mark the model as "ready"
              -- so Ema will begin rendering content in place of "Loading..."
              -- indicator
              readyOnTemplates = bool id M.modelReadyForView (refreshAction == EmaFS.Existing)
          act <- fmap (M.modelHeistTemplate %~) $ do
            s <- readRefreshedFile refreshAction fpAbs
            logD $ "Read " <> show (BS.length s) <> " bytes of template"
            pure $ T.addTemplateFile fpAbs fp s
          pure $ readyOnTemplates >>> act
        EmaFS.Delete -> do
          log $ "Removing template: " <> toText fp
          pure $ M.modelHeistTemplate %~ T.removeTemplateFile fp
    R.AnyExt -> do
      case R.mkRouteFromFilePath fp of
        Nothing ->
          pure id
        Just r -> case action of
          EmaFS.Refresh refreshAction overlays -> do
            let fpAbs = locResolve $ head overlays
            doesDirectoryExist fpAbs >>= \case
              True ->
                -- A directory got added; this is not a static 'file'
                pure id
              False -> do
                let logF = case refreshAction of
                      EmaFS.Existing -> logD . ("Registering" <>)
                      _ -> log . ("Re-registering" <>)
                logF $ " file: " <> toText fpAbs <> " " <> show r
                t <- liftIO getCurrentTime
                pure $ M.modelInsertStaticFile t r fpAbs
          EmaFS.Delete -> do
            pure $ M.modelDeleteStaticFile r

readRefreshedFile :: (MonadLogger m, MonadIO m) => EmaFS.RefreshAction -> FilePath -> m ByteString
readRefreshedFile refreshAction fp =
  case refreshAction of
    EmaFS.Existing -> do
      logD $ "Loading file: " <> toText fp
      readFileBS fp
    _ ->
      readFileFollowingFsnotify fp

-- | Like `readFileBS` but accounts for file truncation due to us responding
-- *immediately* to a fsnotify modify event (which is triggered even before the
-- writer *finishes* writing the new contents). We solve this "glitch" by
-- delaying the read retry, expecting (hoping really) that *this time* the new
-- non-empty contents will come through. 'tis a bit of a HACK though.
readFileFollowingFsnotify :: (MonadIO m, MonadLogger m) => FilePath -> m ByteString
readFileFollowingFsnotify fp = do
  log $ "Reading file: " <> toText fp
  readFileBS fp >>= \case
    "" ->
      reReadFileBS 100 fp >>= \case
        "" ->
          -- Sometimes 100ms is not enough (eg: on WSL), so wait a bit more and
          -- give it another try.
          reReadFileBS 300 fp
        s -> pure s
    s -> pure s
  where
    -- Wait before reading, logging the given delay.
    reReadFileBS ms filePath = do
      threadDelay $ 1000 * ms
      log $ "Re-reading (" <> show ms <> "ms" <> ") file: " <> toText filePath
      readFileBS filePath
