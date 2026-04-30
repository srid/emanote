{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Emanote.Model.Link.Rel where

import Commonmark.Extensions.WikiLink qualified as WL
import Data.Aeson (ToJSON)
import Data.IxSet.Typed (Indexable (..), IxSet, ixFun, ixList)
import Data.IxSet.Typed qualified as Ix
import Data.List.NonEmpty qualified as NEL
import Data.Text qualified as T
import Emanote.Model.Note (Note, noteDoc, noteResolveLinkBase, noteRoute)
import Emanote.Pandoc.Link qualified as Link
import Emanote.Route (LMLRoute, ModelRoute)
import Emanote.Route qualified as R
import Emanote.Route.SiteRoute.Type qualified as SR
import Optics.Operators as Lens ((^.))
import Optics.TH (makeLenses)
import Relude
import System.FilePath (normalise, (</>))
import Text.Pandoc.Definition qualified as B
import Text.Pandoc.Walk qualified as W

-- | Source-order position of a relation inside its containing note.
newtype RelOrder = RelOrder Int
  deriving stock (Eq, Ord, Show)

{- | A relation from one note to anywhere in the model.

 Target will remain unresolved in the `Rel`, and can be resolved at a latter
 time (eg: during rendering).
-}
data Rel = Rel
  { -- The note containing this relation
    _relFrom :: LMLRoute
  , -- The source-order occurrence index inside the containing note.
    _relOrder :: RelOrder
  , -- The target of the relation (can be a note or anything)
    _relTo :: UnresolvedRelTarget
  , _relCtx :: [B.Block]
  -- ^ The relation context in LML
  }
  deriving stock (Eq, Show)

instance Ord Rel where
  compare =
    comparing $ \rel ->
      ( _relFrom rel
      , _relOrder rel
      , _relTo rel
      , _relCtx rel
      )

-- | Key for ordering relation contexts as they appeared in a source note.
relSourceOrder :: Rel -> (LMLRoute, RelOrder)
relSourceOrder rel =
  (_relFrom rel, _relOrder rel)

-- | A Pandoc link together with the block context used for backlinks.
data LinkOccurrence = LinkOccurrence
  { linkOccurrenceUrl :: Text
  , linkOccurrenceAttrs :: [(Text, Text)]
  , linkOccurrenceContext :: [B.Block]
  }

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
  extractLinksInOrder . queryLinksWithContextInOrder $ note ^. noteDoc
  where
    extractLinksInOrder :: [LinkOccurrence] -> IxRel
    extractLinksInOrder links =
      Ix.fromList
        $ flip mapMaybe (zip (RelOrder <$> [0 :: Int ..]) links)
        $ \(order, LinkOccurrence url attrs ctx) -> do
          let parentR = noteResolveLinkBase note
          (target, _manchor) <- parseUnresolvedRelTarget parentR attrs url
          pure $ Rel (note ^. noteRoute) order target ctx

    queryLinksWithContextInOrder :: B.Pandoc -> [LinkOccurrence]
    queryLinksWithContextInOrder =
      W.query blockLinks

    blockLinks :: B.Block -> [LinkOccurrence]
    blockLinks blk =
      case blk of
        B.Para is ->
          queryLinkOccurrences [blk] is
        B.Plain is ->
          queryLinkOccurrences [blk] is
        B.LineBlock is ->
          queryLinkOccurrences [blk] is
        B.Header _ _ is ->
          queryLinkOccurrences [blk] is
        _ -> mempty

    queryLinkOccurrences :: (W.Walkable B.Inline b) => [B.Block] -> b -> [LinkOccurrence]
    queryLinkOccurrences ctx =
      W.query (maybeToList . linkOccurrenceIn ctx)

    linkOccurrenceIn :: [B.Block] -> B.Inline -> Maybe LinkOccurrence
    linkOccurrenceIn ctx inl = do
      (Link.InlineLink, (_, _, attrs), _label, (url, title)) <- Link.parseInlineRef inl
      -- Put title in attrs, as it *is* an attribute.
      pure $ LinkOccurrence url (("title", title) : attrs) ctx

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
