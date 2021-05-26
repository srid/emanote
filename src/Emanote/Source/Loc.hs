-- | Notebook location
module Emanote.Source.Loc where

import qualified Paths_emanote
import System.FilePath ((</>))

-- | Location of the notebook, even if it contains a subset of files.
--
-- The order here matters. Top = higher precedence.
data Loc
  = -- | This always refers to current working directory
    LocUser
  | -- | The location of the emanote's default files directory containing
    -- templates, data, etc.
    LocEmanoteDefault FilePath
  deriving (Eq, Ord, Show)

-- | Return the location layers to union-mount
locLayers :: MonadIO m => m (Set (Loc, FilePath))
locLayers = do
  defaultFiles <- liftIO Paths_emanote.getDataDir
  pure $
    fromList
      [ (LocUser, "."),
        (LocEmanoteDefault defaultFiles, defaultFiles)
      ]

-- | Return the effective path of a file.
locResolve :: (Loc, FilePath) -> FilePath
locResolve (loc, fp) = case loc of
  LocUser -> fp
  LocEmanoteDefault base -> base </> fp
