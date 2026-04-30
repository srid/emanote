{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Emanote.Model.Link.Rel where

import Commonmark.Extensions.WikiLink qualified as WL
import Data.Aeson (ToJSON)
import Data.IxSet.Typed (Indexable (..), IxSet, ixFun, ixList)
import Data.IxSet.Typed qualified as Ix
import Data.List.NonEmpty qualified as NEL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Emanote.Model.Note (Note, noteDoc, noteResolveLinkBase, noteRoute)
import Emanote.Route (LMLRoute, ModelRoute)
import Emanote.Route qualified as R
import Emanote.Route.SiteRoute.Type qualified as SR
import Optics.Operators as Lens ((^.))
import Optics.TH (makeLenses)
import Relude
import System.FilePath (normalise, (</>))
import Text.Pandoc.Definition qualified as B
import Text.Pandoc.LinkContext qualified as LC

{- | A relation from one note to anywhere in the model.

 Target will remain unresolved in the `Rel`, and can be resolved at a latter
 time (eg: during rendering).
-}

-- NOTE: Field order below is load-bearing. The derived 'Ord' compares
-- fields top-to-bottom, and the issue-#186 fix relies on '_relSrcPos'
-- preceding '_relCtx' so source position breaks ties between
-- same-@(_relFrom, _relTo)@ rels before lexicographic 'Ord' on the
-- context blocks does. Reorder only if you also rewrite 'Ord' explicitly.
-- 'Emanote.Model.Link.RelSpec' guards both halves of this invariant.
data Rel = Rel
  { -- The note containing this relation
    _relFrom :: LMLRoute
  , -- The target of the relation (can be a note or anything)
    _relTo :: UnresolvedRelTarget
  , _relSrcPos :: Int
  -- ^ Presentation-layer artifact, not a graph-semantics fact. Consumed
  -- by 'Emanote.Model.Graph.modelLookupBacklinks' (transitively, via
  -- 'IxSet.toList'\'s 'Ord'-driven order) to render backlink context
  -- cards in the order their links appeared in the source note. Other
  -- consumers of 'Rel' do not depend on this field; treat it as opaque.
  -- Tie-breaker index assigned in 'noteRels' construction order. Within
  -- the rels that share @(_relFrom, _relTo)@, this preserves the order in
  -- which 'Text.Pandoc.LinkContext.queryLinksWithContext' yielded their
  -- contexts — i.e. source order for multiple links to the same target
  -- from one note. Across distinct @_relTo@ values this index is /not/
  -- document-wide source position (the underlying @Map@ is keyed by URL,
  -- so the flattening visits URLs alphabetically); rely on it only as a
  -- per-@(_relFrom, _relTo)@ tie-break. Carried so the derived 'Ord'
  -- breaks ties between same-source-target rels by that order, not by
  -- lexicographic 'Ord' on '_relCtx'. This is what keeps backlink
  -- contexts in source order in the rendered UI (issue #186), and what
  -- prevents two same-paragraph links to the same target from collapsing
  -- under @IxSet.fromList@'s set-dedup.
  , _relCtx :: [B.Block]
  -- ^ The relation context in LML
  }
  deriving stock (Eq, Ord, Show)

{- | A link target that has not been resolved (using model) yet.

 Resolving this may or may not result in a resource in the model. The ADT
 constructors capture the different possible types of links the user is
 allowed to link to.
-}
data UnresolvedRelTarget
  = URTWikiLink (WL.WikiLinkType, WL.WikiLink)
  | -- | One or more candidate `ModelRoute`s the URL could resolve to,
    -- ordered by preference. A single URL can map to multiple kinds
    -- (e.g. a @.xml@ URL → feed-enabled note OR static @.xml@ asset);
    -- resolution picks the first candidate that exists in the model.
    URTResource (NonEmpty ModelRoute)
  | URTVirtual SR.VirtualRoute
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (ToJSON)

type RelIxs = '[LMLRoute, UnresolvedRelTarget]

type IxRel = IxSet RelIxs Rel

instance Indexable RelIxs Rel where
  indices =
    ixList
      (ixFun $ one . _relFrom)
      (ixFun $ one . _relTo)

makeLenses ''Rel

noteRels :: Note -> IxRel
noteRels note =
  extractLinks . LC.queryLinksWithContext $ note ^. noteDoc
  where
    -- The 'zipWith [0 ..]' below assigns '_relSrcPos' across the flattened
    -- @(url, instance)@ stream so that, after 'Ix.fromList' \\\\ 'Ix.toList',
    -- rels sharing @(_relFrom, _relTo)@ come out in source-traversal order.
    --
    -- 'queryLinksWithContext' yields its per-URL 'NonEmpty' in /reverse/
    -- traversal order: it folds the @W.query@-emitted list through
    -- 'Map.fromListWith (<>)', whose combining function is applied as
    -- @f new old@, so each later-traversed entry is prepended onto the
    -- accumulator. Reverse the per-URL list back to forward order before
    -- zipping so 'srcPos' counts up with the source. The 'NonEmpty' is
    -- bounded by per-URL link count in a single note — small in practice.
    --
    -- The /across-URL/ ordering is alphabetical because @Map.toList@
    -- iterates by key — fine for the issue-#186 invariant (same-target
    -- multi-link ordering) but /not/ document-wide traversal order. The
    -- '_relSrcPos' haddock documents this for downstream readers.
    extractLinks :: Map Text (NonEmpty ([(Text, Text)], [B.Block])) -> IxRel
    extractLinks m =
      let parentR = noteResolveLinkBase note
          links = do
            (url, instances) <- Map.toList m
            (attrs, ctx) <- reverse (toList instances)
            (target, _manchor) <- maybeToList $ parseUnresolvedRelTarget parentR attrs url
            pure (target, ctx)
       in Ix.fromList $ zipWith mkRel [0 ..] links
      where
        mkRel srcPos (target, ctx) = Rel (note ^. noteRoute) target srcPos ctx

{- | All `UnresolvedRelTarget`s that could resolve to the given
`ModelRoute`. The `URTResource` form is built from the canonical URL
form of @r@ re-parsed via `mkModelRouteCandidates` so the candidate
list matches what the parse path stored in the rel index — without
that round-trip, a static-@.xml@ backlink lookup would search for
@URTResource (one r)@ while parsed rels actually carry the joint
@(feed-route :| [static])@ list and never match.
-}
unresolvedRelsTo :: ModelRoute -> [UnresolvedRelTarget]
unresolvedRelsTo r =
  let allowedWikiLinks = WL.allowedWikiLinks . R.unRoute
      wls = either (R.withLmlRoute allowedWikiLinks . snd) allowedWikiLinks $ R.modelRouteCase r
      resourceURTs =
        maybeToList
          $ viaNonEmpty URTResource (R.mkModelRouteCandidates (R.encodeModelRoute r))
   in (URTWikiLink <$> toList wls) <> resourceURTs

{- | Parse a relative URL string for later resolution.

 TODO: Need tests for this function.
-}
parseUnresolvedRelTarget :: Maybe (R.R 'R.Folder) -> [(Text, Text)] -> Text -> Maybe (UnresolvedRelTarget, Maybe WL.Anchor)
parseUnresolvedRelTarget baseDir attrs url = do
  (wlRes, manchor) <- WL.delineateLink attrs url
  res <- case wlRes of
    Left wl ->
      pure $ URTWikiLink wl
    Right fp ->
      fmap URTVirtual (SR.decodeVirtualRoute fp)
        <|> viaNonEmpty
          URTResource
          ( fp
              & relocateRelUrlUnder (R.encodeRoute <$> baseDir)
              & R.mkModelRouteCandidates
          )
  pure (res, manchor)

relocateRelUrlUnder :: Maybe FilePath -> FilePath -> FilePath
relocateRelUrlUnder mbase fp =
  normalizeIgnoringSymlinks
    $ case mbase of
      Nothing -> fp
      Just x -> x </> fp

-- | Like `System.FilePath.normalise` but also normalises '..'
normalizeIgnoringSymlinks :: FilePath -> FilePath
normalizeIgnoringSymlinks = dropDotDot . normalise

-- Remove '..' from path component.
--
-- `System.FilePath.normalize` ought to do this already, but it doesn't due to
-- symlinks (which we don't use anyway.)
--
-- See https://github.com/haskell/filepath/issues/87
dropDotDot :: FilePath -> FilePath
dropDotDot =
  let go :: Int -> NonEmpty Text -> [Text]
      go n = \case
        (".." :| xs) -> maybe [] (go $ n + 1) $ nonEmpty xs
        (x :| xs) | n == 0 -> x : maybe [] (go 0) (nonEmpty xs)
        x -> maybe [] (go 0) $ nonEmpty $ NEL.drop n x
   in toString . T.intercalate "/" . maybe [] (reverse . go 0 . NEL.reverse) . nonEmpty . T.splitOn "/" . toText

{- | An `UnresolvedRelTarget` that has been resolved.

 See @Model.Link.Resolve@ for actual resolution logic.
-}
data ResolvedRelTarget a
  = RRTMissing
  | RRTAmbiguous (NonEmpty a)
  | RRTFound a
  deriving stock (Eq, Show, Ord, Functor, Generic)
  deriving anyclass (ToJSON)

getResolved :: ResolvedRelTarget a -> Maybe a
getResolved = \case
  RRTFound x -> Just x
  _ -> Nothing

-- This 'a' is either
-- - Note, or
-- - Either (LMLView, Note) StaticFile
resolvedRelTargetFromCandidates :: [a] -> ResolvedRelTarget a
resolvedRelTargetFromCandidates xs =
  case nonEmpty xs of
    Nothing ->
      RRTMissing
    Just (x :| []) ->
      RRTFound x
    Just xs' ->
      RRTAmbiguous xs'

-- | Try to resolve the RRTAmbiguous using the given function.
withAmbiguityResolvedMaybe ::
  (NonEmpty a -> Maybe a) ->
  ResolvedRelTarget a ->
  ResolvedRelTarget a
withAmbiguityResolvedMaybe f = \case
  x@(RRTAmbiguous xs) -> maybe x RRTFound $ f xs
  x -> x
