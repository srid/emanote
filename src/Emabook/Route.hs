{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

module Emabook.Route where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Ema (Slug)
import qualified Ema
import System.FilePath (splitExtension, splitPath)
import qualified Text.Show (Show (show))

-- | Represents the relative path to a source (.md) file under some directory.
--
-- This will also be our site route type.  That is, `Ema.routeUrl (r ::
-- MarkdownRoute)` gives us the URL to the generated HTML for this markdown file.
--
-- If you are using this repo as a template, you might want to use an ADT as
-- route (eg: data Route = Index | About)
newtype MarkdownRoute = MarkdownRoute {unMarkdownRoute :: NonEmpty Slug}
  deriving (Eq, Ord)

instance Show MarkdownRoute where
  show (MarkdownRoute slugs) =
    toString $ "md:" <> T.intercalate "/" (toList $ fmap Ema.unSlug slugs)

newtype BadRoute = BadRoute MarkdownRoute
  deriving (Show, Exception)

-- | Represents the top-level index.md
indexMarkdownRoute :: MarkdownRoute
indexMarkdownRoute = MarkdownRoute $ "index" :| []

-- | Convert foo/bar.md to a @MarkdownRoute@
--
-- If the file is not a Markdown file, return Nothing.
mkMarkdownRouteFromFilePath :: FilePath -> Maybe MarkdownRoute
mkMarkdownRouteFromFilePath = \case
  (splitExtension -> (fp, ".md")) ->
    let slugs = fromString . toString . T.dropWhileEnd (== '/') . toText <$> splitPath fp
     in MarkdownRoute <$> nonEmpty slugs
  _ ->
    Nothing

markdownRouteSourcePath :: MarkdownRoute -> FilePath
markdownRouteSourcePath r =
  if r == indexMarkdownRoute
    then "index.md"
    else toString (T.intercalate "/" $ fmap Ema.unSlug $ toList $ unMarkdownRoute r) <> ".md"

-- | Filename of the markdown file without extension
markdownRouteFileBase :: MarkdownRoute -> Text
markdownRouteFileBase =
  Ema.unSlug . head . NE.reverse . unMarkdownRoute

-- | For use in breadcrumbs
markdownRouteInits :: MarkdownRoute -> NonEmpty MarkdownRoute
markdownRouteInits (MarkdownRoute ("index" :| [])) =
  one indexMarkdownRoute
markdownRouteInits (MarkdownRoute (slug :| rest')) =
  indexMarkdownRoute :| case nonEmpty rest' of
    Nothing ->
      one $ MarkdownRoute (one slug)
    Just rest ->
      MarkdownRoute (one slug) : go (one slug) rest
  where
    go :: NonEmpty Slug -> NonEmpty Slug -> [MarkdownRoute]
    go x (y :| ys') =
      let this = MarkdownRoute (x <> one y)
       in case nonEmpty ys' of
            Nothing ->
              one this
            Just ys ->
              this : go (unMarkdownRoute this) ys
