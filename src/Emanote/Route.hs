{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.Route where

import Data.Aeson (ToJSON)
import Data.Data (Data)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Ema (Slug)
import qualified Ema
import Emanote.Route.Ext (FileType (Html), HasExt (..))
import System.FilePath (splitPath)
import qualified Text.Show (Show (show))

-- | Represents the relative path to some file.
newtype Route (ext :: FileType) = Route {unRoute :: NonEmpty Slug}
  deriving (Eq, Ord, Typeable, Data, Generic, ToJSON)

instance HasExt ext => Show (Route ext) where
  show (Route slugs) =
    toString $
      "R["
        <> show (fileType @ext)
        <> "]:"
        <> T.intercalate "/" (toList $ fmap Ema.unSlug slugs)

-- | Convert foo/bar.<ext> to a @Route@
mkRouteFromFilePath :: forall ext. HasExt ext => FilePath -> Maybe (Route ext)
mkRouteFromFilePath fp = do
  base <- withoutKnownExt @ext fp
  let slugs = fromString . toString . T.dropWhileEnd (== '/') . toText <$> splitPath base
  Route <$> nonEmpty slugs

-- | The base name of the route without its parent path.
routeBaseName :: Route ext -> Text
routeBaseName =
  Ema.unSlug . head . NE.reverse . unRoute

-- | For use in breadcrumbs
routeInits :: Route ext -> NonEmpty (Route ext)
routeInits = \case
  (Route ("index" :| [])) ->
    one indexRoute
  (Route (slug :| rest')) ->
    indexRoute :| case nonEmpty rest' of
      Nothing ->
        one $ Route (one slug)
      Just rest ->
        Route (one slug) : go (one slug) rest
  where
    go :: NonEmpty Slug -> NonEmpty Slug -> [Route ext]
    go x (y :| ys') =
      let this = Route (x <> one y)
       in case nonEmpty ys' of
            Nothing ->
              one this
            Just ys ->
              this : go (unRoute this) ys
    indexRoute :: Route ext
    indexRoute = Route $ "index" :| []

-- | Convert a route to html filepath
encodeRoute :: forall ft. HasExt ft => Route ft -> FilePath
encodeRoute (Route slugs) =
  let parts = Ema.unSlug <$> slugs
   in withExt @ft $ toString $ T.intercalate "/" (toList parts)

-- | Parse our route from html file path
decodeHtmlRoute :: FilePath -> Maybe (Route 'Html)
decodeHtmlRoute fp = do
  if null fp
    then pure $ Route $ one "index"
    else do
      let base = fromMaybe (toText fp) $ T.stripSuffix ".html" (toText fp)
      parts <- nonEmpty $ T.splitOn "/" base
      pure $ Route $ fmap Ema.decodeSlug parts
