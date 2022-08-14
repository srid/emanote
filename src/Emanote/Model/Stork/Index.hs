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
import Emanote.Prelude (log, logD, logW)
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
      index <- runStork input
      atomically $ modifyTVar' indexVar $ \_ -> Just index
      log "STORK: Done generating search index"
      pure index

storkBin :: FilePath
storkBin = $(staticWhich "stork")

runStork :: MonadIO m => Input -> m LByteString
runStork input = do
  let storkToml = Toml.encode inputCodec input
  (_, !index, _) <-
    liftIO $
      readProcessWithExitCode
        storkBin
        -- NOTE: Cannot use "--output -" due to bug in Rust or Stork:
        -- https://github.com/jameslittle230/stork/issues/262
        ["build", "-t", "--input", "-", "--output", "/dev/stdout"]
        (encodeUtf8 storkToml)
  pure $ toLazy index

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
    <$> Toml.string "path" .= filePath
    <*> Toml.text "url" .= fileUrl
    <*> Toml.text "title" .= fileTitle

inputCodec :: TomlCodec Input
inputCodec =
  Input
    <$> Toml.list fileCodec "input.files" .= inputFiles
