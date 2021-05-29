module Emanote.Model.Query where

import Control.Lens.Operators
import Data.IxSet.Typed ((@=))
import qualified Data.IxSet.Typed as Ix
import Emanote.Model.Note
import Emanote.Model.Type

queryByTag :: Model -> Text -> [Note]
queryByTag model tag =
  Ix.toList $ (model ^. modelNotes) @= tag