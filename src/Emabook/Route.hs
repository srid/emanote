{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Emabook.Route where

import Data.Aeson (ToJSON)
import Data.Data (Data)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Data.Text as T
import Ema (Slug)
import qualified Ema
import System.FilePath (splitExtension, splitPath)
import qualified Text.Show (Show (show))

data Md
  deriving (Generic, Data, ToJSON)

data Yaml
  deriving (Generic, Data, ToJSON)

class Ext a where
  getExt :: Proxy a -> String

instance Ext Md where
  getExt Proxy = ".md"

instance Ext Yaml where
  getExt Proxy = ".yaml"

-- | Represents the relative path to a source (.md) file under some directory.
--
-- This will also be our site route type.  That is, `Ema.routeUrl (r ::
-- Route)` gives us the URL to the generated HTML for this markdown file.
--
-- If you are using this repo as a template, you might want to use an ADT as
-- route (eg: data Route = Index | About)
newtype Route ext = Route {unRoute :: NonEmpty Slug}
  deriving (Eq, Ord, Data, Generic, ToJSON)

type MarkdownRoute = Route Md

instance Ext ext => Show (Route ext) where
  show (Route slugs) =
    toString $ "R[" <> toText (getExt (Proxy @ext)) <> "]:" <> T.intercalate "/" (toList $ fmap Ema.unSlug slugs)

newtype BadRoute ext = BadRoute (Route ext)
  deriving (Show, Exception)

-- | Represents the top-level index.md
indexRoute :: Route ext
indexRoute = Route $ "index" :| []

-- | Convert foo/bar.md to a @Route@
--
-- If the file is not a Markdown file, return Nothing.
mkRouteFromFilePath :: forall ext. Ext ext => FilePath -> Maybe (Route ext)
mkRouteFromFilePath fp = do
  let (base, ext) = splitExtension fp
  guard $ ext == getExt (Proxy @ext)
  let slugs = fromString . toString . T.dropWhileEnd (== '/') . toText <$> splitPath base
   in Route <$> nonEmpty slugs

routeSourcePath :: forall ext. Ext ext => Route ext -> FilePath
routeSourcePath r =
  if r == indexRoute
    then "index" <> getExt (Proxy @ext)
    else toString (T.intercalate "/" $ fmap Ema.unSlug $ toList $ unRoute r) <> ".md"

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

-- | Represents the "Foo" in [[Foo]]
--
-- As wiki links may contain multiple path components, it can also represent
-- [[Foo/Bar]], hence we use nonempty slug list.
newtype WikiLinkTarget = WikiLinkTarget {unWikiLinkText :: NonEmpty Slug}
  deriving (Eq, Show, Ord, Data)

mkWikiLinkTargetFromUrl :: Text -> Maybe WikiLinkTarget
mkWikiLinkTargetFromUrl s = do
  guard $ not $ "://" `T.isInfixOf` s
  slugs <- nonEmpty $ Ema.decodeSlug <$> T.splitOn "/" s
  pure $ WikiLinkTarget slugs

-- | Return the various ways to link to this markdown route
--
-- Foo/Bar/Qux.md -> [[Qux]], [[Bar/Qux]], [[Foo/Bar/Qux]]
allowedWikiLinkTargets :: Route Md -> Set WikiLinkTarget
allowedWikiLinkTargets =
  Set.fromList . mapMaybe (fmap WikiLinkTarget . nonEmpty) . toList . NE.tails . unRoute
