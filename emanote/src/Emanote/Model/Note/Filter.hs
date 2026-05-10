module Emanote.Model.Note.Filter (applyDeclaredPandocFilters) where

import Control.Monad.Logger (MonadLogger)
import Control.Monad.Writer.Strict (MonadWriter (tell))
import Data.Default (def)
import Emanote.Prelude (logE)
import Relude
import System.Directory (doesFileExist, doesPathExist)
import System.FilePath (takeExtension, (</>))
import Text.Pandoc (runIO)
import Text.Pandoc.Definition (Pandoc (..))
import Text.Pandoc.Filter qualified as PF
import Text.Pandoc.Scripting (ScriptingEngine)
import UnliftIO.Exception (handle)

{- | Resolve and apply a note's declared Lua filters.

Returns the requested filter paths unchanged alongside the filtered document so
the source-dependency index records the declaration form, not the resolved
absolute path.
-}
applyDeclaredPandocFilters ::
  (MonadIO m, MonadLogger m, MonadWriter [Text] m) =>
  ScriptingEngine ->
  [FilePath] ->
  [FilePath] ->
  Pandoc ->
  m (Pandoc, [FilePath])
applyDeclaredPandocFilters scriptingEngine pluginBaseDir requestedFilters doc = do
  resolvedFilters <- resolvePandocFilterPaths pluginBaseDir requestedFilters
  filteredDoc <- applyPandocFilters scriptingEngine resolvedFilters doc
  pure (filteredDoc, requestedFilters)

resolvePandocFilterPaths ::
  (MonadIO m, MonadWriter [Text] m) =>
  [FilePath] ->
  [FilePath] ->
  m [FilePath]
resolvePandocFilterPaths pluginBaseDir requestedFilters =
  fmap catMaybes $ forM requestedFilters $ \p -> do
    res :: [FilePath] <- flip mapMaybeM pluginBaseDir $ \baseDir -> do
      liftIO (doesPathExist $ baseDir </> p) >>= \case
        False -> do
          pure Nothing
        True ->
          pure $ Just $ baseDir </> p
    case res of
      [] -> do
        tell [toText $ "Pandoc filter " <> p <> " not found in any of: " <> show pluginBaseDir]
        pure Nothing
      (x : _) -> pure $ Just x

applyPandocFilters :: (MonadIO m, MonadLogger m, MonadWriter [Text] m) => ScriptingEngine -> [FilePath] -> Pandoc -> m Pandoc
applyPandocFilters scriptingEngine paths doc = do
  res <- traverse mkLuaFilter paths
  forM_ (lefts res) $ \err ->
    tell [err]
  case rights res of
    [] ->
      pure doc
    filters ->
      applyPandocLuaFilters scriptingEngine filters doc >>= \case
        Left err -> tell [err] >> pure doc
        Right x -> pure x

mkLuaFilter :: (MonadIO m) => FilePath -> m (Either Text PF.Filter)
mkLuaFilter relPath = do
  if takeExtension relPath == ".lua"
    then do
      liftIO (doesFileExist relPath) >>= \case
        True -> pure $ Right $ PF.LuaFilter relPath
        False -> pure $ Left $ toText $ "Lua filter missing: " <> relPath
    else pure $ Left $ "Unsupported filter: " <> toText relPath

applyPandocLuaFilters :: (MonadIO m, MonadLogger m) => ScriptingEngine -> [PF.Filter] -> Pandoc -> m (Either Text Pandoc)
applyPandocLuaFilters scriptingEngine filters x = do
  -- TODO: Can we constrain this to run Lua code purely (embedded) without using IO?
  liftIO (runIOCatchingErrors $ PF.applyFilters scriptingEngine def filters ["markdown"] x) >>= \case
    Left err -> do
      logE $ "Error applying pandoc filters: " <> show err
      pure $ Left (show err)
    Right x' -> pure $ Right x'
  where
    -- `runIO` can throw `PandocError`. Fix this nonsense behaviour, by catching
    -- it and returning a `Left`.
    runIOCatchingErrors =
      handle (pure . Left) . runIO
