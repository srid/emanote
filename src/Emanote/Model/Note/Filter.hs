module Emanote.Model.Note.Filter (applyPandocFilters) where

import Control.Monad.Except
import Control.Monad.Writer.Strict
import Data.Default (def)
import Relude
import System.Directory (doesFileExist)
import System.FilePath (takeExtension)
import Text.Pandoc (runIO)
import Text.Pandoc.Builder qualified as B
import Text.Pandoc.Definition (Pandoc (..))
import Text.Pandoc.Filter qualified as PF

-- TODO: The inline errors should be gathered in model, and reported during
-- static site generation.
applyPandocFilters :: MonadIO m => [FilePath] -> Pandoc -> m Pandoc
applyPandocFilters paths doc = do
  (doc', errs :: [Text]) <- runWriterT $ do
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
  if null errs
    then pure doc'
    else pure $ pandocPrepend (errorDiv errs) doc'

mkLuaFilter :: MonadIO m => FilePath -> m (Either Text PF.Filter)
mkLuaFilter relPath = do
  if takeExtension relPath == ".lua"
    then do
      liftIO (doesFileExist relPath) >>= \case
        True -> pure $ Right $ PF.LuaFilter relPath
        False -> pure $ Left $ toText $ relPath <> " is missing"
    else pure $ Left $ "Unsupported filter: " <> toText relPath

applyPandocLuaFilters :: (MonadIO m) => [PF.Filter] -> Pandoc -> m (Either Text Pandoc)
applyPandocLuaFilters filters x = do
  liftIO (runIO $ PF.applyFilters def filters ["markdown"] x) >>= \case
    Left err -> pure $ Left (show err)
    Right x' -> pure $ Right x'

-- | Prepend to block to the beginning of a Pandoc document (never before H1)
pandocPrepend :: B.Block -> Pandoc -> Pandoc
pandocPrepend prefix (Pandoc meta blocks) =
  let blocks' = case blocks of
        (h1@(B.Header 1 _ _) : rest) ->
          h1 : prefix : rest
        _ -> prefix : blocks
   in Pandoc meta blocks'

errorDiv :: [Text] -> B.Block
errorDiv s =
  B.Div (cls "emanote:error") $ B.Para [B.Strong $ one $ B.Str "Emanote Errors"] : (B.Para . one . B.Str <$> s)
  where
    cls x = ("", one x, mempty) :: B.Attr
