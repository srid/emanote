module Emanote.Model.Link.Resolve where

import Commonmark.Extensions.WikiLink qualified as WL
import Data.List.NonEmpty qualified as NE
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
  resolveUnresolvedRelTarget model (rel ^. Rel.relFrom) (rel ^. Rel.relTo)

resolveUnresolvedRelTarget ::
  Model ->
  -- | Current note route; used to resolve ambiguous links
  R.LMLRoute ->
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
  R.LMLRoute ->
  WL.WikiLink ->
  Rel.ResolvedRelTarget (Either (R.LMLView, MN.Note) SF.StaticFile)
resolveWikiLinkMustExist model currentRoute wl =
  Rel.resolvedRelTargetFromCandidates (M.modelWikiLinkTargets wl model)
    & resolveAmb
  where
    resolveAmb = Rel.withAmbiguityResolvedMaybe (resolveAmbiguity currentRoute)

{- | Resolve ambiguity by selecting the closest common ancestor.

This enables us to merge different notebooks (with similar note filenames) at
the top-level.
-}
resolveAmbiguity ::
  R.LMLRoute ->
  NonEmpty (Either (R.LMLView, MN.Note) SF.StaticFile) ->
  Maybe (Either (R.LMLView, MN.Note) SF.StaticFile)
resolveAmbiguity (R.withLmlRoute coerce -> currentRoute) candidates = do
  -- A function to compute the common ancestor with `currentRoute`
  let f = R.commonAncestor currentRoute . either (R.withLmlRoute coerce . MN._noteRoute . snd) SF._staticFileRoute
  let (a :| as) = NE.sortWith (Down . f) candidates
  -- If the next candidate has the same common ancestor, we have a true
  -- ambiguity. Bail out in that case.
  guard $ viaNonEmpty (f . head) as /= Just (f a)
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
