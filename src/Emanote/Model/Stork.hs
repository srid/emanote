module Emanote.Model.Stork
  ( renderStorkIndex,
  )
where

import Control.Monad.Logger (MonadLoggerIO)
import Data.IxSet.Typed qualified as Ix
import Emanote.Model.Note qualified as N
import Emanote.Model.Stork.Index (File (File), Input (Input), readOrBuildStorkIndex)
import Emanote.Model.Title qualified as Tit
import Emanote.Model.Type (Model)
import Emanote.Model.Type qualified as M
import Emanote.Route qualified as R
import Emanote.Route.SiteRoute qualified as SR
import Emanote.Source.Loc qualified as Loc
import Optics.Core ((^.))
import Relude
import System.FilePath ((</>))

renderStorkIndex :: (MonadIO m, MonadLoggerIO m) => Model -> m LByteString
renderStorkIndex model = do
  readOrBuildStorkIndex (model ^. M.modelStorkIndex) (Input $ storkFiles model)

storkFiles :: Model -> [File]
storkFiles model =
  let baseDir = Loc.locPath . Loc.primaryLayer $ model ^. M.modelLayers
   in Ix.toList (model ^. M.modelNotes) <&> \note ->
        File
          ((baseDir </>) $ R.withLmlRoute R.encodeRoute $ note ^. N.noteRoute)
          (SR.siteRouteUrl model $ SR.lmlSiteRoute $ note ^. N.noteRoute)
          (Tit.toPlain $ note ^. N.noteTitle)
