-- | Notebook location
module Emanote.Source.Loc
  ( -- * Type
    Loc (..),

    -- * Making a `Loc`
    defaultLayer,
    userLayers,

    -- * Using a `Loc`
    locResolve,
  )
where

import Relude
import System.FilePath ((</>))

-- | Location of the notebook
--
-- The order here matters. Top = higher precedence.
data Loc
  = -- | The Int argument specifies the precedence (larger value = higher precedence)
    LocUser Int FilePath
  | -- | The default location (ie., emanote default layer)
    LocDefault FilePath
  deriving stock (Eq, Ord, Show)

defaultLayer :: FilePath -> (Loc, FilePath)
defaultLayer fp =
  (LocDefault fp, fp)

userLayers :: NonEmpty FilePath -> Set (Loc, FilePath)
userLayers paths =
  fromList $
    zip [1 ..] (toList paths) <&> \(idx, path) ->
      (LocUser idx path, path)

-- | Return the effective path of a file.
locResolve :: (Loc, FilePath) -> FilePath
locResolve (loc, fp) = locPath loc </> fp

locPath :: Loc -> FilePath
locPath = \case
  LocUser _ fp -> fp
  LocDefault fp -> fp
