{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Emanote.Model.Stork.Index
  ( IndexVar,
    newIndex,
    clearStorkIndex,
    readOrBuildStorkIndex,
    File (File),
    Input (Input),
  )
where

import Control.Monad.Logger (MonadLoggerIO)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import Emanote.Prelude (log, logD, logW)
import Numeric (showGFloat)
import Relude
import System.Process.ByteString (readProcessWithExitCode)
import System.Which (staticWhich)
import Toml (TomlCodec, encode, list, string, text, (.=))

-- | In-memory Stork index tracked in a @TVar@
newtype IndexVar = IndexVar (TVar (Maybe LByteString))

newIndex :: MonadIO m => m IndexVar
newIndex =
  IndexVar <$> newTVarIO mempty

clearStorkIndex :: (MonadIO m) => IndexVar -> m ()
clearStorkIndex (IndexVar var) = atomically $ writeTVar var mempty

readOrBuildStorkIndex :: (MonadIO m, MonadLoggerIO m) => IndexVar -> Input -> m LByteString
readOrBuildStorkIndex (IndexVar indexVar) input = do
  readTVarIO indexVar >>= \case
    Just index -> do
      logD "STORK: Returning cached search index"
      pure index
    Nothing -> do
      -- TODO: What if there are concurrent reads? We probably need a lock.
      -- And we want to encapsulate this whole thing.
      logW "STORK: Generating search index (this may be expensive)"
      (diff, !index) <- timeIt $ runStork input
      log $ toText $ "STORK: Done generating search index in " <> showGFloat (Just 2) diff "" <> " seconds"
      atomically $ modifyTVar' indexVar $ \_ -> Just index
      pure index
  where
    timeIt :: MonadIO m => m b -> m (Double, b)
    timeIt m = do
      t0 <- liftIO getCurrentTime
      !x <- m
      t1 <- liftIO getCurrentTime
      let diff :: NominalDiffTime = diffUTCTime t1 t0
      pure (realToFrac diff, x)

storkBin :: FilePath
storkBin = $(staticWhich "stork")

runStork :: MonadIO m => Input -> m LByteString
runStork input = do
  let storkToml = handleTomlandBug $ Toml.encode inputCodec input
  (_, !index, _) <-
    liftIO $
      readProcessWithExitCode
        storkBin
        -- NOTE: Cannot use "--output -" due to bug in Rust or Stork:
        -- https://github.com/jameslittle230/stork/issues/262
        ["build", "-t", "--input", "-", "--output", "/dev/stdout"]
        (encodeUtf8 storkToml)
  pure $ toLazy index
  where
    handleTomlandBug =
      -- HACK: Deal with tomland's bug.
      -- https://github.com/srid/emanote/issues/336
      -- https://github.com/kowainik/tomland/issues/408
      --
      -- This could be problematic if the user literally uses \\U in their note
      -- title (but why would they?)
      T.replace "\\\\U" "\\U"

newtype Input = Input
  { inputFiles :: [File]
  }
  deriving stock (Eq, Show)

data File = File
  { filePath :: FilePath,
    fileUrl :: Text,
    fileTitle :: Text
  }
  deriving stock (Eq, Show)

fileCodec :: TomlCodec File
fileCodec =
  File
    <$> Toml.string "path"
    .= filePath
    <*> Toml.text "url"
    .= fileUrl
    <*> Toml.text "title"
    .= fileTitle

inputCodec :: TomlCodec Input
inputCodec =
  Input
    <$> Toml.list fileCodec "input.files"
    .= inputFiles
