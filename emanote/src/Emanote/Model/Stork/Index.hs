{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Emanote.Model.Stork.Index (
  IndexVar,
  newIndex,
  clearStorkIndex,
  readOrBuildStorkIndex,
  File (File),
  Input (Input),
  Config (Config),
  Handling,
  FileType (..),
) where

import Control.Monad.Logger (MonadLoggerIO)
import Data.Default (Default (..))
import Data.Text qualified as T
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import Deriving.Aeson
import Emanote.Prelude (log, logD, logW)
import Numeric (showGFloat)
import Relude
import System.Process.ByteString (readProcessWithExitCode)
import System.Which (staticWhich)
import Toml (Key, TomlCodec, diwrap, encode, list, string, table, text, textBy, (.=))

-- | In-memory Stork index tracked in a @TVar@
newtype IndexVar = IndexVar (TVar (Maybe LByteString))

newIndex :: (MonadIO m) => m IndexVar
newIndex =
  IndexVar <$> newTVarIO mempty

clearStorkIndex :: (MonadIO m) => IndexVar -> m ()
clearStorkIndex (IndexVar var) = atomically $ writeTVar var mempty

readOrBuildStorkIndex :: (MonadIO m, MonadLoggerIO m) => IndexVar -> Config -> m LByteString
readOrBuildStorkIndex (IndexVar indexVar) config = do
  readTVarIO indexVar >>= \case
    Just index -> do
      logD "STORK: Returning cached search index"
      pure index
    Nothing -> do
      -- TODO: What if there are concurrent reads? We probably need a lock.
      -- And we want to encapsulate this whole thing.
      logW "STORK: Generating search index (this may be expensive)"
      (diff, !index) <- timeIt $ runStork config
      log $ toText $ "STORK: Done generating search index in " <> showGFloat (Just 2) diff "" <> " seconds"
      atomically $ modifyTVar' indexVar $ \_ -> Just index
      pure index
  where
    timeIt :: (MonadIO m) => m b -> m (Double, b)
    timeIt m = do
      t0 <- liftIO getCurrentTime
      !x <- m
      t1 <- liftIO getCurrentTime
      let diff :: NominalDiffTime = diffUTCTime t1 t0
      pure (realToFrac diff, x)

storkBin :: FilePath
storkBin = $(staticWhich "stork")

runStork :: (MonadIO m) => Config -> m LByteString
runStork config = do
  let storkToml = handleTomlandBug $ Toml.encode configCodec config
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

newtype Config = Config
  { configInput :: Input
  }
  deriving stock (Eq, Show)

data Input = Input
  { inputFiles :: [File]
  , inputFrontmatterHandling :: Handling
  }
  deriving stock (Eq, Show)

data File = File
  { filePath :: FilePath
  , fileUrl :: Text
  , fileTitle :: Text
  , fileFiletype :: FileType
  }
  deriving stock (Eq, Show)

data FileType
  = FileType_PlainText
  | FileType_Markdown
  deriving stock (Eq, Show, Generic)
  deriving
    (FromJSON)
    via CustomJSON
          '[ ConstructorTagModifier '[StripPrefix "FileType_", CamelToSnake]
           ]
          FileType

data Handling
  = Handling_Ignore
  | Handling_Omit
  | Handling_Parse
  deriving stock (Eq, Show, Generic)
  deriving
    (FromJSON)
    via CustomJSON
          '[ ConstructorTagModifier '[StripPrefix "Handling_", CamelToSnake]
           ]
          Handling

instance Default Handling where
  def = Handling_Omit

configCodec :: TomlCodec Config
configCodec =
  Config
    <$> Toml.table inputCodec "input"
      .= configInput
  where
    inputCodec :: TomlCodec Input
    inputCodec =
      Input
        <$> Toml.list fileCodec "files"
          .= inputFiles
        <*> Toml.diwrap (handlingCodec "frontmatter_handling")
          .= inputFrontmatterHandling
    fileCodec :: TomlCodec File
    fileCodec =
      File
        <$> Toml.string "path"
          .= filePath
        <*> Toml.text "url"
          .= fileUrl
        <*> Toml.text "title"
          .= fileTitle
        <*> Toml.diwrap (filetypeCodec "filetype")
          .= fileFiletype
    handlingCodec :: Toml.Key -> TomlCodec Handling
    handlingCodec = textBy showHandling parseHandling
      where
        showHandling :: Handling -> Text
        showHandling handling = case handling of
          Handling_Ignore -> "Ignore"
          Handling_Omit -> "Omit"
          Handling_Parse -> "Parse"
        parseHandling :: Text -> Either Text Handling
        parseHandling handling = case handling of
          "Ignore" -> Right Handling_Ignore
          "Omit" -> Right Handling_Omit
          "Parse" -> Right Handling_Parse
          other -> Left $ "Unsupported value for frontmatter handling: " <> other
    filetypeCodec :: Toml.Key -> TomlCodec FileType
    filetypeCodec = textBy showFileType parseFileType
      where
        showFileType :: FileType -> Text
        showFileType filetype = case filetype of
          FileType_PlainText -> "PlainText"
          FileType_Markdown -> "Markdown"
        parseFileType :: Text -> Either Text FileType
        parseFileType filetype = case filetype of
          "PlainText" -> Right FileType_PlainText
          "Markdown" -> Right FileType_Markdown
          other -> Left $ "Unsupported value for filetype: " <> other
