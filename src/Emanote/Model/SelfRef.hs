module Emanote.Model.SelfRef where

import Emanote.Route.SomeRoute (SomeRoute)
import qualified Emanote.Route.WikiLink as WL

-- | Any potential WikiLink that refer to a note.
newtype SelfRef = SelfRef {unSelfRef :: WL.WikiLink}
  deriving (Eq, Ord, Show)

routeSelfRefs :: SomeRoute -> [SelfRef]
routeSelfRefs =
  fmap SelfRef . toList
    . WL.allowedWikiLinks
