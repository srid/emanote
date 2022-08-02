{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Emanote.View.Stork
  ( renderStorkIndex,
  )
where

import Control.Monad.Logger (MonadLoggerIO)
import Data.IxSet.Typed qualified as Ix
import Emanote.Model (Model)
import Emanote.Model.Note qualified as N
import Emanote.Model.Title qualified as Tit
import Emanote.Model.Type qualified as M
import Emanote.Prelude (log, logW)
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
  -- TODO: this should retrieve from cache if model hasn't changed
  logW "Generating search index using Stork (this may be expensive)"
  let storkToml = Toml.encode inputCodec $ Input $ storkFiles model
  (_, !index, _) <- liftIO $ readProcessWithExitCode storkBin ["build", "-t", "--input", "-", "--output", "-"] (encodeUtf8 storkToml)
  log "Done generating Stork index"
  pure $ toLazy index

storkFiles :: Model -> [File]
storkFiles model =
  let baseDir = Loc.locPath . Loc.primaryLayer $ model ^. M.modelLayers
   in Ix.toList (model ^. M.modelNotes) <&> \note ->
        File
          ((baseDir </>) $ R.withLmlRoute R.encodeRoute $ note ^. N.noteRoute)
          (toText $ R.encodeRoute $ N.noteHtmlRoute note)
          (Tit.toPlain $ note ^. N.noteTitle)
