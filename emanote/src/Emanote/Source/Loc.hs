{-# LANGUAGE DeriveAnyClass #-}

-- | Notebook location
module Emanote.Source.Loc (
  -- * Type
  Loc (..),

  -- * Making a `Loc`
  defaultLayer,
  userLayers,

  -- * Using a `Loc`
  locResolve,
  locPath,
  locMountPoint,

  -- * Dealing with layers of locs
  userLayersToSearch,
) where

import Data.Set qualified as Set
import Deriving.Aeson qualified as Aeson
import Relude
import System.FilePath ((</>))

{- | Location of the notebook

 The order here matters. Top = higher precedence.
-}
data Loc
  = -- | The Int argument specifies the precedence (lower value = higher precedence). The last argument is "mount point"
    LocUser Int FilePath (Maybe FilePath)
  | -- | The default location (ie., emanote default layer)
    LocDefault FilePath
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Aeson.ToJSON)

{- | List of user layers, highest precedent being at first.

This is useful to delay searching for content in layers.
-}
userLayersToSearch :: Set Loc -> [FilePath]
userLayersToSearch =
  mapMaybe
    ( \case
        LocUser _ fp _ -> Just fp
        LocDefault _ -> Nothing
    )
    . Set.toAscList

defaultLayer :: FilePath -> Loc
defaultLayer = LocDefault

userLayers :: NonEmpty (FilePath, Maybe FilePath) -> Set Loc
userLayers paths =
  fromList
    $ zip [1 ..] (toList paths)
    <&> (\(a, (b, c)) -> LocUser a b c)

-- | Return the effective path of a file.
locResolve :: (Loc, FilePath) -> FilePath
locResolve (loc, fp) = fst (locPath loc) </> fp

locPath :: Loc -> (FilePath, Maybe FilePath)
locPath = \case
  LocUser _ fp m -> (fp, m)
  LocDefault fp -> (fp, Nothing)

locMountPoint :: Loc -> Maybe FilePath
locMountPoint = \case
  LocUser _ _ mountPoint -> mountPoint
  LocDefault _ -> Nothing
