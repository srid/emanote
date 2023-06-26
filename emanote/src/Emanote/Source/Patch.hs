-- | Patch model state depending on file change event.
module Emanote.Source.Patch (
  patchModel,
  filePatterns,
  ignorePatterns,
) where

import Control.Exception (throwIO)
import Control.Monad.Logger (LoggingT (runLoggingT), MonadLogger, MonadLoggerIO (askLoggerIO))
import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NEL
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Emanote.Model qualified as M
import Emanote.Model.Note qualified as N
import Emanote.Model.SData qualified as SD
import Emanote.Model.StaticFile (readStaticFileInfo)
import Emanote.Model.Stork.Index qualified as Stork
import Emanote.Model.Type (ModelEma)
import Emanote.Prelude (
  BadInput (BadInput),
  log,
  logD,
 )
import Emanote.Route qualified as R
import Emanote.Source.Loc (Loc, LocLayers, locPath, locResolve, primaryLayer)
import Emanote.Source.Pattern (filePatterns, ignorePatterns)
import Heist.Extra.TemplateState qualified as T
import Optics.Operators ((%~))
import Relude
import Relude.Extra (traverseToSnd)
import System.UnionMount qualified as UM
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Directory (doesDirectoryExist)

-- | Map a filesystem change to the corresponding model change.
patchModel ::
  (MonadIO m, MonadLogger m, MonadLoggerIO m) =>
  LocLayers ->
  (N.Note -> N.Note) ->
  Stork.IndexVar ->
  -- | Type of the file being changed
  R.FileType R.SourceExt ->
  -- | Path to the file being changed
  FilePath ->
  -- | Specific change to the file, along with its paths from other "layers"
  UM.FileAction (NonEmpty (Loc, FilePath)) ->
  m (ModelEma -> ModelEma)
patchModel layers noteF storkIndexTVar fpType fp action = do
  logger <- askLoggerIO
  now <- liftIO getCurrentTime
  -- Prefix all patch logging with timestamp.
  let newLogger loc src lvl s =
        logger loc src lvl $ fromString (formatTime defaultTimeLocale "[%H:%M:%S] " now) <> s
  runLoggingT (patchModel' layers noteF storkIndexTVar fpType fp action) newLogger

-- | Map a filesystem change to the corresponding model change.
patchModel' ::
  (MonadIO m, MonadLogger m) =>
  LocLayers ->
  (N.Note -> N.Note) ->
  Stork.IndexVar ->
  -- | Type of the file being changed
  R.FileType R.SourceExt ->
  -- | Path to the file being changed
  FilePath ->
  -- | Specific change to the file, along with its paths from other "layers"
  UM.FileAction (NonEmpty (Loc, FilePath)) ->
  m (ModelEma -> ModelEma)
patchModel' layers noteF storkIndexTVar fpType fp action = do
  case fpType of
    R.LMLType lmlType -> do
      case R.mkLMLRouteFromKnownFilePath lmlType fp of
        Nothing ->
          pure id -- Impossible
        Just r -> do
          -- Stork doesn't support incremental building of index, so we must
          -- clear it to pave way for a rebuild later when requested.
          --
          -- From https://github.com/jameslittle230/stork/discussions/112#discussioncomment-252861
          --
          -- > Stork also doesn't support incremental index updates today --
          -- you'd have to re-index everything when users added a new document,
          -- which might be prohibitively long.
          Stork.clearStorkIndex storkIndexTVar

          case action of
            UM.Refresh refreshAction overlays -> do
              let fpAbs = locResolve $ head overlays
                  -- TODO: This should automatically be computed, instead of being passed.
                  -- We need access to the model though! With dependency management to boot.
                  -- Until this, `layers` is threaded through as a hack.
                  currentLayerPath = locPath $ primaryLayer layers
              s <- readRefreshedFile refreshAction fpAbs
              note <- N.parseNote currentLayerPath r fpAbs (decodeUtf8 s)
              pure $ M.modelInsertNote $ noteF note
            UM.Delete -> do
              log $ "Removing note: " <> toText fp
              pure $ M.modelDeleteNote r
    R.Yaml ->
      case R.mkRouteFromFilePath fp of
        Nothing ->
          pure id
        Just r -> case action of
          UM.Refresh refreshAction overlays -> do
            yamlContents <- forM (NEL.reverse overlays) $ \overlay -> do
              let fpAbs = locResolve overlay
              traverseToSnd (readRefreshedFile refreshAction) fpAbs
            sData <-
              liftIO $
                either (throwIO . BadInput) pure $
                  SD.parseSDataCascading r yamlContents
            pure $ M.modelInsertData sData
          UM.Delete -> do
            log $ "Removing data: " <> toText fp
            pure $ M.modelDeleteData r
    R.HeistTpl ->
      case action of
        UM.Refresh refreshAction overlays -> do
          let fpAbs = locResolve $ head overlays
              -- Once we start loading HTML templates, mark the model as "ready"
              -- so Ema will begin rendering content in place of "Loading..."
              -- indicator
              readyOnTemplates = bool id M.modelReadyForView (refreshAction == UM.Existing)
          act <- do
            s <- readRefreshedFile refreshAction fpAbs
            logD $ "Read " <> show (BS.length s) <> " bytes of template"
            pure $ M.modelHeistTemplate %~ T.addTemplateFile fpAbs fp s
          pure $ readyOnTemplates >>> act
        UM.Delete -> do
          log $ "Removing template: " <> toText fp
          pure $ M.modelHeistTemplate %~ T.removeTemplateFile fp
    R.AnyExt -> do
      case R.mkRouteFromFilePath fp of
        Nothing ->
          pure id
        Just r -> case action of
          UM.Refresh refreshAction overlays -> do
            let fpAbs = locResolve $ head overlays
            doesDirectoryExist fpAbs >>= \case
              True ->
                -- A directory got added; this is not a static 'file'
                pure id
              False -> do
                let logF = case refreshAction of
                      UM.Existing -> logD . ("Registering" <>)
                      _ -> log . ("Re-registering" <>)
                logF $ " file: " <> toText fpAbs <> " " <> show r
                t <- liftIO getCurrentTime
                mInfo <- readStaticFileInfo fpAbs (fmap decodeUtf8 . readRefreshedFile refreshAction)
                pure $ M.modelInsertStaticFile t r fpAbs mInfo
          UM.Delete -> do
            pure $ M.modelDeleteStaticFile r

readRefreshedFile :: (MonadLogger m, MonadIO m) => UM.RefreshAction -> FilePath -> m ByteString
readRefreshedFile refreshAction fp =
  case refreshAction of
    UM.Existing -> do
      logD $ "Loading file: " <> toText fp
      readFileBS fp
    _ ->
      readFileFollowingFsnotify fp

{- | Like `readFileBS` but accounts for file truncation due to us responding
 *immediately* to a fsnotify modify event (which is triggered even before the
 writer *finishes* writing the new contents). We solve this "glitch" by
 delaying the read retry, expecting (hoping really) that *this time* the new
 non-empty contents will come through. 'tis a bit of a HACK though.
-}
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
