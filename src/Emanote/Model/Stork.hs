{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Emanote.Model.Stork
  ( renderStorkIndex,
    runStork,
  )
where

import Control.Monad.Logger (MonadLoggerIO)
import Data.IxSet.Typed qualified as Ix
import Emanote.Model (Model)
import Emanote.Model.Note qualified as N
import Emanote.Model.Title qualified as Tit
import Emanote.Model.Type qualified as M
import Emanote.Prelude (log, logD, logW)
import Emanote.Route qualified as R
import Emanote.Source.Loc qualified as Loc
import Optics.Core ((^.))
import Relude
import System.FilePath ((</>))
import System.Process.ByteString (readProcessWithExitCode)
import System.Which (staticWhich)
import Toml (TomlCodec, encode, list, string, text, (.=))

storkBin :: FilePath
storkBin = $(staticWhich "stork")

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

renderStorkIndex :: (MonadIO m, MonadLoggerIO m) => Model -> m LByteString
renderStorkIndex model = do
  let indexTVar = model ^. M.modelStorkIndex
  readTVarIO indexTVar >>= \case
    Just index -> do
      logD "STORK: Returning cached search index"
      pure index
    Nothing -> do
      -- TODO: What if there are concurrent reads? We probably need a lock.
      -- And we want to encapsulate this whole thing.
      logW "STORK: Generating search index (this may be expensive)"
      index <- runStork $ Input $ storkFiles model
      atomically $ modifyTVar' indexTVar $ \_ -> Just index
      log "STORK: Done generating search index"
      pure index

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

storkFiles :: Model -> [File]
storkFiles model =
  let baseDir = Loc.locPath . Loc.primaryLayer $ model ^. M.modelLayers
   in Ix.toList (model ^. M.modelNotes) <&> \note ->
        File
          ((baseDir </>) $ R.withLmlRoute R.encodeRoute $ note ^. N.noteRoute)
          (toText $ R.encodeRoute $ N.noteHtmlRoute note)
          (Tit.toPlain $ note ^. N.noteTitle)
