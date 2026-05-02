{-# LANGUAGE DeriveAnyClass #-}

module Emanote.Route.SiteRoute.Type (
  SiteRoute (..),
  VirtualRoute (..),
  ExportFormat (..),
  ResourceRoute (..),
  decodeVirtualRoute,
  encodeVirtualRoute,
  encodeTagIndexR,
  encodeTagIndexUrl,
) where

import Data.Aeson (ToJSON)
import Data.Text qualified as T
import Emanote.Pandoc.Markdown.Syntax.HashTag qualified as HT
import Emanote.Route.Ext qualified as Ext
import Emanote.Route.ModelRoute (LMLRoute, LMLView, StaticFileRoute, lmlRouteCase)
import Emanote.Route.R qualified as R
import Network.URI.Slug qualified as Slug
import Relude hiding (show)
import Text.Show (show)

data ExportFormat
  = ExportFormat_Metadata
  | ExportFormat_Content
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

-- | A route to a virtual resource (not in `Model`)
data VirtualRoute
  = VirtualRoute_Index
  | VirtualRoute_TagIndex [HT.TagNode]
  | VirtualRoute_Export ExportFormat
  | VirtualRoute_StorkIndex
  | VirtualRoute_TaskIndex
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

{- | A route to a resource in `Model`

 This is *mostly isomorphic* to `ModelRoute`, except for containing the
 absolute path to the static file.
-}
data ResourceRoute
  = ResourceRoute_StaticFile StaticFileRoute FilePath
  | ResourceRoute_LML LMLView LMLRoute
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (ToJSON)

data SiteRoute
  = SiteRoute_VirtualRoute VirtualRoute
  | SiteRoute_ResourceRoute ResourceRoute
  | SiteRoute_MissingR FilePath
  | SiteRoute_AmbiguousR FilePath (NonEmpty LMLRoute)
  deriving stock (Eq, Ord, Generic)

instance Show SiteRoute where
  show = \case
    SiteRoute_MissingR urlPath ->
      "404: " <> urlPath
    SiteRoute_AmbiguousR urlPath _notes ->
      "Amb: " <> urlPath
    SiteRoute_ResourceRoute rr ->
      case rr of
        ResourceRoute_StaticFile r _fp ->
          show r
        ResourceRoute_LML _view r ->
          show $ lmlRouteCase r
    SiteRoute_VirtualRoute x ->
      show x

decodeVirtualRoute :: FilePath -> Maybe VirtualRoute
decodeVirtualRoute fp =
  (VirtualRoute_Index <$ decodeIndexR fp)
    <|> (VirtualRoute_TagIndex <$> decodeTagIndexR fp)
    <|> (VirtualRoute_Export <$> decodeExportR fp)
    <|> (VirtualRoute_StorkIndex <$ decodeStorkIndexR fp)
    <|> (VirtualRoute_TaskIndex <$ decodeTaskIndexR fp)

decodeIndexR :: FilePath -> Maybe ()
decodeIndexR fp = do
  "-" :| ["all"] <- pure $ R.unRoute $ R.decodeHtmlRoute fp
  pass

decodeExportR :: FilePath -> Maybe ExportFormat
decodeExportR fp = do
  "-" :| [file] <- R.unRoute <$> R.decodeAnyRoute fp
  case file of
    "export.json" -> pure ExportFormat_Metadata
    "export.md" -> pure ExportFormat_Content
    _ -> Nothing

decodeStorkIndexR :: FilePath -> Maybe ()
decodeStorkIndexR fp = do
  "-" :| ["stork.st"] <- R.unRoute <$> R.decodeAnyRoute fp
  pass

decodeTagIndexR :: FilePath -> Maybe [HT.TagNode]
decodeTagIndexR fp = do
  "-" :| "tags" : tagPath <- pure $ R.unRoute $ R.decodeHtmlRoute fp
  pure $ fmap (HT.TagNode . Slug.unSlug) tagPath

decodeTaskIndexR :: FilePath -> Maybe ()
decodeTaskIndexR fp = do
  "-" :| ["tasks"] <- pure $ R.unRoute $ R.decodeHtmlRoute fp
  pass

-- NOTE: The sentinel route slugs in this function should match with those of
-- the decoders above.
encodeVirtualRoute :: VirtualRoute -> FilePath
encodeVirtualRoute = \case
  VirtualRoute_TagIndex tagNodes ->
    R.encodeRoute $ encodeTagIndexR tagNodes
  VirtualRoute_Index ->
    R.encodeRoute $ R.R @() @'Ext.Html $ "-" :| ["all"]
  VirtualRoute_Export exportFormat ->
    R.encodeRoute $ R.R @Ext.SourceExt @'Ext.AnyExt $ "-" :| [encodeExportR exportFormat]
  VirtualRoute_StorkIndex ->
    R.encodeRoute $ R.R @Ext.SourceExt @'Ext.AnyExt $ "-" :| ["stork.st"]
  VirtualRoute_TaskIndex ->
    R.encodeRoute $ R.R @() @'Ext.Html $ "-" :| ["tasks"]

encodeExportR :: ExportFormat -> Slug.Slug
encodeExportR = \case
  ExportFormat_Metadata -> fromString "export.json"
  ExportFormat_Content -> fromString "export.md"

encodeTagIndexR :: [HT.TagNode] -> R.R 'Ext.Html
encodeTagIndexR tagNodes =
  R.R $ "-" :| "tags" : fmap (fromString . toString . HT.unTagNode) tagNodes

{- | URL form of `encodeTagIndexR` that percent-encodes each path segment.

Tags can contain characters reserved by RFC 3986 — most importantly @\#@,
which Zettelkasten conventions use to mark "structure note" tags like
@##§1@. Without encoding, the literal @\#@ in the @href@ truncates the
URL into a fragment in the browser. See #199.

Exists because the Pandoc filter that linkifies inline @\#tag@ syntax
('Emanote.Pandoc.BuiltinFilters.linkifyInlineTags') has no @Model@
reachable, so it cannot route through 'Emanote.Route.SiteRoute.Class.siteRouteUrl'
(which percent-encodes via Ema's 'Ema.routeUrlWith' /
@filepathToUrl@). Anywhere a @Model@ is in scope — e.g. the
'Emanote.View.Common.commonSplices' tag-list splice or
'Emanote.View.TagIndex' breadcrumbs — prefer @siteRouteUrl@ and let
Ema's encoder do the work.

Invariant: for any @tagNodes@, this must agree with
@siteRouteUrl model (tagIndexRoute tagNodes)@ when the model's URL
strategy is 'Ema.UrlDirect' (the Pandoc-filter site-wide assumption —
inline tag links are always emitted with the @.html@ suffix because
the filter cannot read the per-site URL strategy). If 'Slug.encodeSlug'
or 'encodeTagIndexR' changes, both this function and Ema's encoder must
keep producing the same output.
-}
encodeTagIndexUrl :: [HT.TagNode] -> Text
encodeTagIndexUrl tagNodes =
  let parts = Slug.encodeSlug <$> R.unRoute (encodeTagIndexR tagNodes)
   in T.intercalate "/" (toList parts) <> ".html"
