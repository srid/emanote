module Emanote.Model.Link.Resolve where

import Control.Lens.Operators ((^.))
import Data.Time (UTCTime)
import Data.WorldPeace.Union (openUnionLift)
import qualified Emanote.Model.Link.Rel as Rel
import qualified Emanote.Model.Note as MN
import qualified Emanote.Model.StaticFile as SF
import Emanote.Model.Type (Model)
import qualified Emanote.Model.Type as M
import qualified Emanote.Pandoc.Markdown.Syntax.WikiLink as WL
import qualified Emanote.Route as R
import qualified Emanote.Route.SiteRoute as SR

-- | TODO: This is bad. "Either Text" (missing or ambiguous) is already encoded in SiteRoute. So use that.
resolveUnresolvedRelTarget ::
  Model -> Rel.UnresolvedRelTarget -> Rel.ResolvedRelTarget (SR.SiteRoute, Maybe UTCTime)
resolveUnresolvedRelTarget model = \case
  Rel.URTWikiLink (_wlType, wl) -> do
    resourceSiteRoute <$> resolveWikiLinkMustExist model wl
  Rel.URTResource r ->
    resourceSiteRoute <$> resolveModelRoute model r
  Rel.URTVirtual virtualRoute -> do
    Rel.RRTFound $
      SR.SiteRoute
        ( openUnionLift virtualRoute
        )
        & (,Nothing)

resolveWikiLinkMustExist :: Model -> WL.WikiLink -> Rel.ResolvedRelTarget (Either MN.Note SF.StaticFile)
resolveWikiLinkMustExist model wl =
  Rel.resolvedRelTargetFromCandidates $ M.modelWikiLinkTargets wl model

resolveModelRoute :: Model -> R.ModelRoute -> Rel.ResolvedRelTarget (Either MN.Note SF.StaticFile)
resolveModelRoute model lr = do
  let eRoute = R.modelRouteCase lr
  bitraverse
    (`M.modelLookupNoteByRoute` model)
    (`M.modelLookupStaticFileByRoute` model)
    eRoute
    & maybe Rel.RRTMissing Rel.RRTFound

resourceSiteRoute :: Either MN.Note SF.StaticFile -> (SR.SiteRoute, Maybe UTCTime)
resourceSiteRoute = \case
  Left note ->
    SR.noteFileSiteRoute note
      & (,Nothing)
  Right sf ->
    SR.staticFileSiteRoute sf
      & (,Just $ sf ^. SF.staticFileTime)
