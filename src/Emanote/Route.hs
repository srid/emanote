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
import Emanote.Route.Ext
import System.FilePath (splitPath)
import qualified Text.Show (Show (show))

-- | Represents the relative path to a source (.md) file under some directory.
--
-- This will also be our site route type.  That is, `Ema.routeUrl (r ::
-- Route)` gives us the URL to the generated HTML for this markdown file.
--
-- If you are using this repo as a template, you might want to use an ADT as
-- route (eg: data Route = Index | About)
newtype Route (ext :: FileType) = Route {unRoute :: NonEmpty Slug}
  deriving (Eq, Ord, Data, Generic, ToJSON)

type MarkdownRoute = Route ('LMLType 'Md)

instance HasExt ext => Show (Route ext) where
  show (Route slugs) =
    toString $
      "R["
        <> show (fileType @ext)
        <> "]:"
        <> T.intercalate "/" (toList $ fmap Ema.unSlug slugs)

-- | Represents the top-level index.md
indexRoute :: Route ext
indexRoute = Route $ "index" :| []

-- | Convert foo/bar.md to a @Route@
--
-- If the file is not a Markdown file, return Nothing.
mkRouteFromFilePath :: forall ext. HasExt ext => FilePath -> Maybe (Route ext)
mkRouteFromFilePath fp = do
  base <- withoutKnownExt @ext fp
  let slugs = fromString . toString . T.dropWhileEnd (== '/') . toText <$> splitPath base
  Route <$> nonEmpty slugs

routeSourcePath :: forall ext. HasExt ext => Route ext -> FilePath
routeSourcePath r =
  withExt @ext $ toString (T.intercalate "/" $ fmap Ema.unSlug $ toList $ unRoute r)

-- | Filename of the markdown file without extension
routeFileBase :: Route ext -> Text
routeFileBase =
  Ema.unSlug . head . NE.reverse . unRoute

-- | For use in breadcrumbs
routeInits :: Route ext -> NonEmpty (Route ext)
routeInits (Route ("index" :| [])) =
  one indexRoute
routeInits (Route (slug :| rest')) =
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

-- | Convert a route to html filepath
encodeRoute :: forall ft. HasExt ft => Route ft -> FilePath
encodeRoute (Route slugs) =
  withExt @ft $ case nonEmpty (Ema.unSlug <$> toList slugs) of
    Nothing -> "index"
    Just parts ->
      toString $ T.intercalate "/" (toList parts)

-- | Parse our route from html file path
-- See FIXME: in Ema.Route's Either instance for FileRoute.
decodeHtmlRoute :: FilePath -> Maybe (Route 'Html)
decodeHtmlRoute fp = do
  if null fp
    then pure $ Route $ one "index"
    else do
      let base = fromMaybe (toText fp) $ T.stripSuffix ".html" (toText fp)
      parts <- nonEmpty $ T.splitOn "/" base
      pure $ Route $ fmap Ema.decodeSlug parts
