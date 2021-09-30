module Emanote.Model.Link.Resolve where

import Data.WorldPeace.Union (openUnionLift)
import qualified Emanote.Model.Link.Rel as Rel
import qualified Emanote.Model.Note as MN
import qualified Emanote.Model.StaticFile as SF
import Emanote.Model.Type (Model)
import qualified Emanote.Model.Type as M
import qualified Emanote.Pandoc.Markdown.Syntax.WikiLink as WL
import qualified Emanote.Route as R
import qualified Emanote.Route.SiteRoute as SR
import Relude

resolveUnresolvedRelTarget ::
  Model ->
  Rel.UnresolvedRelTarget ->
  Rel.ResolvedRelTarget SR.SiteRoute
resolveUnresolvedRelTarget model = \case
  Rel.URTWikiLink (_wlType, wl) -> do
    resolveWikiLinkMustExist model wl
      <&> resourceSiteRoute
  Rel.URTResource r ->
    resolveModelRoute model r
      <&> resourceSiteRoute
  Rel.URTVirtual virtualRoute -> do
    Rel.RRTFound $
      SR.SiteRoute
        ( openUnionLift virtualRoute
        )

resolveWikiLinkMustExist ::
  Model -> WL.WikiLink -> Rel.ResolvedRelTarget (Either MN.Note SF.StaticFile)
resolveWikiLinkMustExist model wl =
  Rel.resolvedRelTargetFromCandidates $ M.modelWikiLinkTargets wl model

resolveModelRoute ::
  Model -> R.ModelRoute -> Rel.ResolvedRelTarget (Either MN.Note SF.StaticFile)
resolveModelRoute model lr =
  bitraverse
    (`M.modelLookupNoteByRoute` model)
    (`M.modelLookupStaticFileByRoute` model)
    (R.modelRouteCase lr)
    & maybe Rel.RRTMissing Rel.RRTFound

resourceSiteRoute :: Either MN.Note SF.StaticFile -> SR.SiteRoute
resourceSiteRoute =
  either SR.noteFileSiteRoute SR.staticFileSiteRoute
