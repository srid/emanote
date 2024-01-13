module Emanote.Model.Link.Resolve where

import Commonmark.Extensions.WikiLink qualified as WL
import Emanote.Model.Link.Rel qualified as Rel
import Emanote.Model.Note qualified as MN
import Emanote.Model.StaticFile qualified as SF
import Emanote.Model.Type (Model)
import Emanote.Model.Type qualified as M
import Emanote.Route qualified as R
import Emanote.Route.SiteRoute qualified as SR
import Optics.Core ((^.))
import Relude

resolveRel :: Model -> Rel.Rel -> Rel.ResolvedRelTarget SR.SiteRoute
resolveRel model rel =
  resolveUnresolvedRelTarget model (Just $ rel ^. Rel.relFrom) (rel ^. Rel.relTo)

resolveUnresolvedRelTarget ::
  Model ->
  -- | Current note route; used to resolve ambiguous links
  Maybe R.LMLRoute ->
  Rel.UnresolvedRelTarget ->
  Rel.ResolvedRelTarget SR.SiteRoute
resolveUnresolvedRelTarget model currentRoute = \case
  Rel.URTWikiLink (_wlType, wl) -> do
    resolveWikiLinkMustExist model currentRoute wl
      <&> resourceSiteRoute
  Rel.URTResource r ->
    resolveModelRoute model r
      <&> resourceSiteRoute
  Rel.URTVirtual virtualRoute -> do
    Rel.RRTFound
      $ SR.SiteRoute_VirtualRoute
        virtualRoute

resolveWikiLinkMustExist ::
  Model ->
  -- | Current note route; used to resolve ambiguous links
  Maybe R.LMLRoute ->
  WL.WikiLink ->
  Rel.ResolvedRelTarget (Either (R.LMLView, MN.Note) SF.StaticFile)
resolveWikiLinkMustExist model mCurrentRoute wl =
  Rel.resolvedRelTargetFromCandidates (Just resolveAmbiguity) $ M.modelWikiLinkTargets wl model
  where
    resolveAmbiguity :: NonEmpty (Either (R.LMLView, MN.Note) SF.StaticFile) -> Maybe (Either (R.LMLView, MN.Note) SF.StaticFile)
    resolveAmbiguity candidates = do
      currentRoute :: R.R ext <- fmap (R.withLmlRoute coerce) mCurrentRoute
      -- traceShow (currentRoute, candidates <&> either (show . MN._noteRoute . snd) show) Nothing
      let k = R.commonAncestor currentRoute . either (R.withLmlRoute coerce . MN._noteRoute . snd) SF._staticFileRoute
      (a :| as) <- nonEmpty $ sortWith (Down . k) (toList candidates)
      -- if there is no *single* maximumBy, bail out.
      guard $ (viaNonEmpty (k . head) as) /= Just (k a)
      pure a

resolveModelRoute ::
  Model -> R.ModelRoute -> Rel.ResolvedRelTarget (Either (R.LMLView, MN.Note) SF.StaticFile)
resolveModelRoute model lr =
  bitraverse
    (`M.modelLookupNoteByRoute` model)
    (`M.modelLookupStaticFileByRoute` model)
    (R.modelRouteCase lr)
    & maybe Rel.RRTMissing Rel.RRTFound

resourceSiteRoute :: Either (R.LMLView, MN.Note) SF.StaticFile -> SR.SiteRoute
resourceSiteRoute =
  either SR.noteFileSiteRoute SR.staticFileSiteRoute
