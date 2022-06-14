{-# LANGUAGE DeriveAnyClass #-}

module Emanote.Route.SiteRoute.Type
  ( SiteRoute (..),
    VirtualRoute (..),
    ResourceRoute,
    decodeVirtualRoute,
    encodeVirtualRoute,
    encodeTagIndexR,
  )
where

import Data.Aeson (ToJSON)
import Data.WorldPeace.Union
  ( OpenUnion,
    absurdUnion,
  )
import Emanote.Pandoc.Markdown.Syntax.HashTag qualified as HT
import Emanote.Prelude (h)
import Emanote.Route.Ext qualified as Ext
import Emanote.Route.ModelRoute (LMLRoute, StaticFileRoute, lmlRouteCase)
import Emanote.Route.R qualified as R
import Network.URI.Slug qualified as Slug
import Relude hiding (show)
import Text.Show (show)

-- | A route to a virtual resource (not in `Model`)
data VirtualRoute
  = VirtualRoute_IndexR
  | VirtualRoute_TagIndexR [HT.TagNode]
  | VirtualRoute_ExportR
  | VirtualRoute_TasksR
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

type ResourceRoute' =
  '[ (StaticFileRoute, FilePath),
     LMLRoute
   ]

-- | A route to a resource in `Model`
--
-- This is *mostly isomorphic* to `ModelRoute`, except for containing the
-- absolute path to the static file.
type ResourceRoute = OpenUnion ResourceRoute'

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
    SiteRoute_ResourceRoute x ->
      x & absurdUnion
        `h` ( \(r :: StaticFileRoute, _fp :: FilePath) ->
                show r
            )
        `h` ( \(r :: LMLRoute) ->
                show $ lmlRouteCase r
            )
    SiteRoute_VirtualRoute x ->
      show x

decodeVirtualRoute :: FilePath -> Maybe VirtualRoute
decodeVirtualRoute fp =
  (VirtualRoute_IndexR <$ decodeIndexR fp)
    <|> (VirtualRoute_TagIndexR <$> decodeTagIndexR fp)
    <|> (VirtualRoute_ExportR <$ decodeExportR fp)
    <|> (VirtualRoute_TasksR <$ decodeTasksR fp)

decodeIndexR :: FilePath -> Maybe ()
decodeIndexR fp = do
  "-" :| ["all"] <- pure $ R.unRoute $ R.decodeHtmlRoute fp
  pass

decodeExportR :: FilePath -> Maybe ()
decodeExportR fp = do
  "-" :| ["export.json"] <- R.unRoute <$> R.decodeAnyRoute fp
  pass

decodeTagIndexR :: FilePath -> Maybe [HT.TagNode]
decodeTagIndexR fp = do
  "-" :| "tags" : tagPath <- pure $ R.unRoute $ R.decodeHtmlRoute fp
  pure $ fmap (HT.TagNode . Slug.unSlug) tagPath

decodeTasksR :: FilePath -> Maybe ()
decodeTasksR fp = do
  "-" :| ["tasks"] <- pure $ R.unRoute $ R.decodeHtmlRoute fp
  pass

-- NOTE: The sentinel route slugs in this function should match with those of
-- the decoders above.
encodeVirtualRoute :: VirtualRoute -> FilePath
encodeVirtualRoute = \case
  VirtualRoute_TagIndexR tagNodes ->
    R.encodeRoute $ encodeTagIndexR tagNodes
  VirtualRoute_IndexR ->
    R.encodeRoute $ R.R @() @'Ext.Html $ "-" :| ["all"]
  VirtualRoute_ExportR ->
    R.encodeRoute $ R.R @Ext.SourceExt @'Ext.AnyExt $ "-" :| ["export.json"]
  VirtualRoute_TasksR ->
    R.encodeRoute $ R.R @() @'Ext.Html $ "-" :| ["tasks"]

encodeTagIndexR :: [HT.TagNode] -> R.R 'Ext.Html
encodeTagIndexR tagNodes =
  R.R $ "-" :| "tags" : fmap (fromString . toString . HT.unTagNode) tagNodes
