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
import Emanote.Model.Note (Note, noteDoc, noteRoute)
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
data Rel = Rel
  { -- The note containing this relation
    _relFrom :: LMLRoute
  , -- The target of the relation (can be a note or anything)
    _relTo :: UnresolvedRelTarget
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
  | URTResource ModelRoute
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
    extractLinks :: Map Text (NonEmpty ([(Text, Text)], [B.Block])) -> IxRel
    extractLinks m =
      Ix.fromList
        $ flip concatMap (Map.toList m)
        $ \(url, instances) -> do
          flip mapMaybe (toList instances) $ \(attrs, ctx) -> do
            let parentR = R.withLmlRoute R.routeParent $ note ^. noteRoute
            (target, _manchor) <- parseUnresolvedRelTarget parentR attrs url
            pure $ Rel (note ^. noteRoute) target ctx

unresolvedRelsTo :: ModelRoute -> [UnresolvedRelTarget]
unresolvedRelsTo r =
  let allowedWikiLinks = WL.allowedWikiLinks . R.unRoute
      wls = either (\(_, r') -> R.withLmlRoute allowedWikiLinks r') allowedWikiLinks $ R.modelRouteCase r
   in (URTWikiLink <$> toList wls)
        <> [URTResource r]

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
        <|> fmap
          URTResource
          ( fp
              & relocateRelUrlUnder (R.encodeRoute <$> baseDir)
              & R.mkModelRouteFromFilePath
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

-- This 'a' is either
-- - Note, or
-- - Either (LMLView, Note) StaticFile
resolvedRelTargetFromCandidates :: Maybe (NonEmpty a -> Maybe a) -> [a] -> ResolvedRelTarget a
resolvedRelTargetFromCandidates mResolveAmbiguity xs =
  case nonEmpty xs of
    Nothing ->
      RRTMissing
    Just (x :| []) ->
      RRTFound x
    Just xs' -> maybe (RRTAmbiguous xs') RRTFound $ do
      f <- mResolveAmbiguity
      f xs'
