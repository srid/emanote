module Emanote.Model.Note.Filter (applyPandocFilters) where

import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Writer.Strict
import Data.Default (def)
import Emanote.Prelude (logE, logW)
import Relude
import System.Directory (doesFileExist)
import System.FilePath (takeExtension)
import Text.Pandoc (runIO)
import Text.Pandoc.Definition (Pandoc (..))
import Text.Pandoc.Filter qualified as PF
import UnliftIO.Exception (handle)

applyPandocFilters :: (MonadIO m, MonadLogger m, MonadWriter [Text] m) => [FilePath] -> Pandoc -> m Pandoc
applyPandocFilters paths doc = do
  res <- traverse mkLuaFilter paths
  forM_ (lefts res) $ \err ->
    tell [err]
  case rights res of
    [] ->
      pure doc
    filters ->
      applyPandocLuaFilters filters doc >>= \case
        Left err -> tell [err] >> pure doc
        Right x -> pure x

mkLuaFilter :: MonadIO m => FilePath -> m (Either Text PF.Filter)
mkLuaFilter relPath = do
  if takeExtension relPath == ".lua"
    then do
      liftIO (doesFileExist relPath) >>= \case
        True -> pure $ Right $ PF.LuaFilter relPath
        False -> pure $ Left $ toText $ relPath <> " is missing"
    else pure $ Left $ "Unsupported filter: " <> toText relPath

applyPandocLuaFilters :: (MonadIO m, MonadLogger m) => [PF.Filter] -> Pandoc -> m (Either Text Pandoc)
applyPandocLuaFilters filters x = do
  logW $ "[Experimental feature] Applying pandoc filters: " <> show filters
  -- TODO: Can we constrain this to run Lua code purely (embedded) without using IO?
  liftIO (runIOCatchingErrors $ PF.applyFilters def filters ["markdown"] x) >>= \case
    Left err -> do
      logE $ "Error applying pandoc filters: " <> show err
      pure $ Left (show err)
    Right x' -> pure $ Right x'
  where
    -- `runIO` can throw `PandocError`. Fix this nonsense behaviour, by catching
    -- it and returning a `Left`.
    runIOCatchingErrors =
      handle (pure . Left) . runIO
