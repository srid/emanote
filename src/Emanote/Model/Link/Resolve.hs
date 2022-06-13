module Emanote.Model.Link.Resolve where

import Emanote.Model.Link.Rel qualified as Rel
import Emanote.Model.Note qualified as MN
import Emanote.Model.StaticFile qualified as SF
import Emanote.Model.Type (Model)
import Emanote.Model.Type qualified as M
import Emanote.Pandoc.Markdown.Syntax.WikiLink qualified as WL
import Emanote.Route qualified as R
import Emanote.Route.SiteRoute qualified as SR
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
      SR.SiteRoute_VirtualRoute
        virtualRoute

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
