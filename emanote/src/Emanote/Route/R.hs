module Emanote.Route.R where

import Data.Aeson (ToJSON (toJSON))
import Data.Data (Data)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Emanote.Route.Ext (FileType (..), HasExt (..))
import Network.URI.Slug (Slug)
import Network.URI.Slug qualified as Slug
import Relude
import System.FilePath (splitPath)
import Text.Show qualified (Show (show))

{- | Represents the relative path to some file (or its isomporphic URL
 represetation).
-}
newtype R (ext :: FileType a) = R {unRoute :: NonEmpty Slug}
  deriving stock (Eq, Ord, Typeable, Data)

instance HasExt ext => ToJSON (R ext) where
  toJSON = toJSON . encodeRoute

instance HasExt ext => Show (R ext) where
  show r =
    toString $
      "R[/" <> encodeRoute r <> "]"

-- | Convert foo/bar.<ext> to a @R@
mkRouteFromFilePath :: forall a (ext :: FileType a). HasExt ext => FilePath -> Maybe (R ext)
mkRouteFromFilePath fp = do
  base <- withoutKnownExt @_ @ext fp
  let slugs = fromString . toString . T.dropWhileEnd (== '/') . toText <$> splitPath base
  viaNonEmpty R slugs

mkRouteFromSlugs :: NonEmpty Slug -> R ext
mkRouteFromSlugs =
  R

-- | If the route is a single-slug URL, return the only slug.
routeSlug :: R ext -> Maybe Slug
routeSlug r = do
  x :| [] <- pure $ unRoute r
  pure x

-- | Like `routeSlug` but skips the given prefixes, returning the (only) pending slug.
routeSlugWithPrefix :: NonEmpty Slug -> R ext -> Maybe Slug
routeSlugWithPrefix prefix r = do
  lastSlug :| (nonEmpty -> Just prevSlugs) <- pure $ NE.reverse $ unRoute r
  guard $ NE.reverse prevSlugs == prefix
  pure lastSlug

-- | The base name of the route without its parent path.
routeBaseName :: R ext -> Text
routeBaseName =
  Slug.unSlug . head . NE.reverse . unRoute

routeParent :: R ext -> Maybe (R 'Folder)
routeParent =
  viaNonEmpty R . init . unRoute

-- | For use in breadcrumbs
routeInits :: R ext -> NonEmpty (R ext)
routeInits = \case
  (R ("index" :| [])) ->
    one indexRoute
  (R (slug :| rest')) ->
    indexRoute :| case nonEmpty rest' of
      Nothing ->
        one $ R (one slug)
      Just rest ->
        R (one slug) : go (one slug) rest
  where
    go :: NonEmpty Slug -> NonEmpty Slug -> [R ext]
    go x (y :| ys') =
      let this = R (x <> one y)
       in case nonEmpty ys' of
            Nothing ->
              one this
            Just ys ->
              this : go (unRoute this) ys

indexRoute :: R ext
indexRoute = R $ "index" :| []

-- | Convert a route to filepath
encodeRoute :: forall a (ft :: FileType a). HasExt ft => R ft -> FilePath
encodeRoute (R slugs) =
  let parts = Slug.unSlug <$> slugs
   in withExt @a @ft $ toString $ T.intercalate "/" (toList parts)

decodeXmlRoute :: FilePath -> Maybe (R 'Xml)
decodeXmlRoute fp = case T.stripSuffix ".xml" (toText fp) of
  Nothing -> Nothing
  Just base -> case T.splitOn "/" base of
    [] -> Nothing
    [""] -> Nothing
    x : xs -> Just $ R $ fmap Slug.decodeSlug (x :| xs)

-- | Parse our route from html file path
decodeHtmlRoute :: FilePath -> R 'Html
decodeHtmlRoute fp = do
  let base = fromMaybe (toText fp) $ T.stripSuffix ".html" (toText fp)
  R $ case splitOnNE "/" base of
    Nothing ->
      one "index"
    Just parts ->
      fmap Slug.decodeSlug parts
  where
    -- Like `T.splitOn` but returns a NonEmpty list with sensible semantics
    splitOnNE k s =
      case T.splitOn k s of
        [] -> Nothing
        [""] -> Nothing
        x : xs -> Just $ x :| xs

decodeAnyRoute :: FilePath -> Maybe (R 'AnyExt)
decodeAnyRoute =
  mkRouteFromFilePath @_ @'AnyExt
