{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.Route.R where

import Data.Aeson (ToJSON)
import Data.Data (Data)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Ema (Slug)
import qualified Ema
import Emanote.Route.Ext (FileType (Html), HasExt (..))
import System.FilePath (splitPath)
import qualified Text.Show (Show (show))

-- | Represents the relative path to some file (or its isomporphic URL
-- represetation).
newtype R (ext :: FileType) = R {unRoute :: NonEmpty Slug}
  deriving (Eq, Ord, Typeable, Data, Generic, ToJSON)

instance HasExt ext => Show (R ext) where
  show (R slugs) =
    toString $
      "R["
        <> show (fileType @ext)
        <> "]:"
        <> T.intercalate "/" (toList $ fmap Ema.unSlug slugs)

-- | Convert foo/bar.<ext> to a @R@
mkRouteFromFilePath :: forall ext. HasExt ext => FilePath -> Maybe (R ext)
mkRouteFromFilePath fp = do
  base <- withoutKnownExt @ext fp
  let slugs = fromString . toString . T.dropWhileEnd (== '/') . toText <$> splitPath base
  R <$> nonEmpty slugs

-- | The base name of the route without its parent path.
routeBaseName :: R ext -> Text
routeBaseName =
  Ema.unSlug . head . NE.reverse . unRoute

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

-- | Convert a route to html filepath
encodeRoute :: forall ft. HasExt ft => R ft -> FilePath
encodeRoute (R slugs) =
  let parts = Ema.unSlug <$> slugs
   in withExt @ft $ toString $ T.intercalate "/" (toList parts)

-- | Parse our route from html file path
decodeHtmlRoute :: FilePath -> Maybe (R 'Html)
decodeHtmlRoute fp = do
  if null fp
    then pure $ R $ one "index"
    else do
      let base = fromMaybe (toText fp) $ T.stripSuffix ".html" (toText fp)
      parts <- nonEmpty $ T.splitOn "/" base
      pure $ R $ fmap Ema.decodeSlug parts
