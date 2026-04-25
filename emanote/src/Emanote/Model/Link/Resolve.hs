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
  case R.modelRouteCase lr of
    -- A `.xml` URL parses to an `LMLView_Atom` route by default (so
    -- `[atom](feed.xml)` can target a feed-enabled note). When no
    -- such note exists, fall back to looking up a static `.xml`
    -- asset of the same path before declaring the link broken
    -- (regression: #547).
    Left (R.LMLView_Atom, lmlR) ->
      case M.modelLookupNoteByRoute (R.LMLView_Atom, lmlR) model of
        Just hit -> Rel.RRTFound (Left hit)
        Nothing ->
          maybe Rel.RRTMissing (Rel.RRTFound . Right)
            $ flip M.modelLookupStaticFileByRoute model
            =<< xmlStaticRouteFromLml lmlR
    other ->
      bitraverse
        (`M.modelLookupNoteByRoute` model)
        (`M.modelLookupStaticFileByRoute` model)
        other
        & maybe Rel.RRTMissing Rel.RRTFound

{- | Reinterpret an `LMLRoute` (parsed from a `.xml` URL into the
Atom-feed slot) as an opaque `.xml` static-file route. The `Maybe`
is only there because `mkRouteFromFilePath` is partial in general; on
the encoded path it is total (a non-empty `NonEmpty Slug` always
encodes to a non-empty filepath that re-parses).
-}
xmlStaticRouteFromLml :: R.LMLRoute -> Maybe (R.R 'R.AnyExt)
xmlStaticRouteFromLml lmlRoute =
  R.mkRouteFromFilePath @_ @'R.AnyExt
    $ R.encodeRoute @_ @'R.Xml
    $ R.withLmlRoute coerce lmlRoute

resourceSiteRoute :: Either (R.LMLView, MN.Note) SF.StaticFile -> SR.SiteRoute
resourceSiteRoute =
  either SR.noteFileSiteRoute SR.staticFileSiteRoute
