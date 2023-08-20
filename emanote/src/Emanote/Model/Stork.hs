module Emanote.Model.Stork (
  renderStorkIndex,
) where

import Control.Monad.Logger (MonadLoggerIO)
import Data.Default (Default (def))
import Data.IxSet.Typed qualified as Ix
import Emanote.Model.Meta (lookupRouteMeta)
import Emanote.Model.Note qualified as N
import Emanote.Model.Stork.Index (
  Config (Config),
  File (File),
  FileType (FileType_Markdown, FileType_PlainText),
  Handling,
  Input (Input),
  readOrBuildStorkIndex,
 )
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
  let config = Config $ Input (storkFiles model) (frontmatterHandling model)
  readOrBuildStorkIndex (model ^. M.modelStorkIndex) config

storkFiles :: Model -> [File]
storkFiles model =
  let baseDir = Loc.locPath . Loc.primaryLayer $ model ^. M.modelLayers
   in Ix.toList (model ^. M.modelNotes) <&> \note ->
        let fp = ((baseDir </>) $ R.withLmlRoute R.encodeRoute $ note ^. N.noteRoute)
            ft = case note ^. N.noteRoute of
              R.LMLRoute_Md _ -> FileType_Markdown
              R.LMLRoute_Org _ -> FileType_PlainText
         in File
              fp
              (SR.siteRouteUrl model $ SR.lmlSiteRoute (R.LMLView_Html, note ^. N.noteRoute))
              (Tit.toPlain $ note ^. N.noteTitle)
              ft

frontmatterHandling :: Model -> Handling
frontmatterHandling model =
  let indexRoute = M.modelIndexRoute model
   in lookupRouteMeta def ("template" :| ["stork", "frontmatter-handling"]) indexRoute model
