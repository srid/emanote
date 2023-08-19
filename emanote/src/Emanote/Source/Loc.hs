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

  -- * Dealing with layers of locs
  LocLayers,
  primaryLayer,
) where

import Data.Set qualified as Set
import Relude
import System.FilePath ((</>))

{- | Location of the notebook

 The order here matters. Top = higher precedence.
-}
data Loc
  = -- | The Int argument specifies the precedence (lower value = higher precedence)
    LocUser Int FilePath
  | -- | The default location (ie., emanote default layer)
    LocDefault FilePath
  deriving stock (Eq, Ord, Show)

type LocLayers = Set Loc

{- | Return the "primary" `LocUser` layer (that which are not overrides).

 Assumes that the user has put it always by last; i.e, `-L foo;primary/layer`.
-}
primaryLayer :: HasCallStack => LocLayers -> Loc
primaryLayer =
  Set.findMax . Set.filter isUserLayer
  where
    isUserLayer = \case
      LocUser _ _ -> True
      _ -> False

defaultLayer :: FilePath -> Loc
defaultLayer = LocDefault

userLayers :: NonEmpty FilePath -> Set Loc
userLayers paths =
  fromList $
    zip [1 ..] (toList paths) <&> uncurry LocUser

-- | Return the effective path of a file.
locResolve :: (Loc, FilePath) -> FilePath
locResolve (loc, fp) = locPath loc </> fp

locPath :: Loc -> FilePath
locPath = \case
  LocUser _ fp -> fp
  LocDefault fp -> fp
