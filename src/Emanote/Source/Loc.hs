-- | Notebook location
module Emanote.Source.Loc where

import qualified Paths_emanote
import System.FilePath ((</>))

-- | Location of the notebook, even if it contains a subset of files.
--
-- The order here matters. Top = higher precedence.
data Loc
  = -- | The Int argument specifies the precedence (large value = higher precedence)
    LocUser Int FilePath
  | -- | The location of the emanote's default files directory containing
    -- templates, data, etc.
    LocEmanoteDefault FilePath
  deriving (Eq, Ord, Show)

emanoteDefaultLayer :: MonadIO m => m (Loc, FilePath)
emanoteDefaultLayer = do
  defaultFiles <- liftIO Paths_emanote.getDataDir
  pure (LocEmanoteDefault defaultFiles, defaultFiles)

userLayers :: NonEmpty FilePath -> Set (Loc, FilePath)
userLayers paths =
  fromList $
    zip [1 ..] (toList paths) <&> \(idx, path) ->
      (LocUser idx path, path)

-- | Return the effective path of a file.
locResolve :: (Loc, FilePath) -> FilePath
locResolve (loc, fp) = case loc of
  LocUser _idx base -> base </> fp
  LocEmanoteDefault base -> base </> fp
