{-# LANGUAGE DeriveAnyClass #-}

module Emanote.Route.SiteRoute.Type
  ( SiteRoute (..),
    IndexR (..),
    ExportR (..),
    TasksR (..),
    QueryR (..),
    TagIndexR (..),
    MissingR (..),
    AmbiguousR (..),
    VirtualRoute,
    ResourceRoute,
    decodeVirtualRoute,
    encodeVirtualRoute,
    encodeTagIndexR,
  )
where

import Data.Aeson (ToJSON)
import Data.Text qualified as T
import Data.WorldPeace.Union
  ( OpenUnion,
    absurdUnion,
    openUnionLift,
  )
import Ema (Slug (unSlug))
import Emanote.Model.Query.Type (Query, parseQuery)
import Emanote.Pandoc.Markdown.Syntax.HashTag qualified as HT
import Emanote.Prelude (h)
import Emanote.Route.Ext qualified as Ext
import Emanote.Route.ModelRoute (LMLRoute, StaticFileRoute, lmlRouteCase)
import Emanote.Route.R qualified as R
import Relude hiding (show)
import Text.Show (show)

data IndexR = IndexR
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (ToJSON)

newtype TagIndexR = TagIndexR [HT.TagNode]
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (ToJSON)

data ExportR = ExportR
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (ToJSON)

data TasksR = TasksR
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (ToJSON)

data QueryR = QueryR Query
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (ToJSON)

-- | A 404 route
newtype MissingR = MissingR {unMissingR :: FilePath}
  deriving stock (Eq, Show, Ord)

-- | An ambiguous route
newtype AmbiguousR = AmbiguousR {unAmbiguousR :: (FilePath, NonEmpty LMLRoute)}
  deriving stock (Eq, Show, Ord)

type VirtualRoute' =
  '[ IndexR,
     TagIndexR,
     ExportR,
     TasksR,
     QueryR
   ]

-- | A route to a virtual resource (not in `Model`)
type VirtualRoute = OpenUnion VirtualRoute'

type ResourceRoute' =
  '[ (StaticFileRoute, FilePath),
     LMLRoute
   ]

-- | A route to a resource in `Model`
--
-- This is *mostly isomorphic* to `ModelRoute`, except for containing the
-- absolute path to the static file.
type ResourceRoute = OpenUnion ResourceRoute'

type SiteRoute' =
  '[ VirtualRoute,
     ResourceRoute,
     MissingR,
     AmbiguousR
   ]

newtype SiteRoute = SiteRoute {unSiteRoute :: OpenUnion SiteRoute'}
  deriving stock (Eq)

instance Show SiteRoute where
  show (SiteRoute sr) =
    sr
      & absurdUnion
      `h` ( \(MissingR urlPath) ->
              "404: " <> urlPath
          )
      `h` ( \(AmbiguousR (urlPath, _notes)) -> do
              "Amb: " <> urlPath
          )
      `h` ( \(x :: ResourceRoute) ->
              x & absurdUnion
                `h` ( \(r :: StaticFileRoute, _fp :: FilePath) ->
                        show r
                    )
                `h` ( \(r :: LMLRoute) ->
                        show $ lmlRouteCase r
                    )
          )
      `h` ( \(x :: VirtualRoute) ->
              show x
          )

decodeVirtualRoute :: FilePath -> Maybe VirtualRoute
decodeVirtualRoute fp =
  fmap openUnionLift (decodeIndexR fp)
    <|> fmap openUnionLift (decodeTagIndexR fp)
    <|> fmap openUnionLift (decodeExportR fp)
    <|> fmap openUnionLift (decodeTasksR fp)
    <|> fmap openUnionLift (decodeQueryR fp)

decodeIndexR :: FilePath -> Maybe IndexR
decodeIndexR fp = do
  "-" :| ["all"] <- pure $ R.unRoute $ R.decodeHtmlRoute fp
  pure IndexR

decodeExportR :: FilePath -> Maybe ExportR
decodeExportR fp = do
  "-" :| ["export.json"] <- R.unRoute <$> R.decodeAnyRoute fp
  pure ExportR

decodeTagIndexR :: FilePath -> Maybe TagIndexR
decodeTagIndexR fp = do
  "-" :| "tags" : tagPath <- pure $ R.unRoute $ R.decodeHtmlRoute fp
  let tagNodes = fmap (HT.TagNode . Ema.unSlug) tagPath
  pure $ TagIndexR tagNodes

decodeTasksR :: FilePath -> Maybe TasksR
decodeTasksR fp = do
  "-" :| ["tasks"] <- pure $ R.unRoute $ R.decodeHtmlRoute fp
  pure TasksR

decodeQueryR :: FilePath -> Maybe QueryR
decodeQueryR fp = do
  "-" :| "query" : x : xs <- pure $ R.unRoute $ R.decodeHtmlRoute fp
  q <- parseQuery $ T.intercalate "/" . fmap unSlug $ x : xs
  pure $ QueryR q

-- NOTE: The sentinel route slugs in this function should match with those of
-- the decoders above.
encodeVirtualRoute :: VirtualRoute -> FilePath
encodeVirtualRoute =
  absurdUnion
    `h` ( \tr@(TagIndexR _) ->
            R.encodeRoute $ encodeTagIndexR tr
        )
    `h` ( \IndexR ->
            R.encodeRoute $ R.R @() @'Ext.Html $ "-" :| ["all"]
        )
    `h` ( \ExportR ->
            R.encodeRoute $ R.R @Ext.SourceExt @'Ext.AnyExt $ "-" :| ["export.json"]
        )
    `h` ( \TasksR ->
            R.encodeRoute $ R.R @() @'Ext.Html $ "-" :| ["tasks"]
        )
    `h` ( \(QueryR q) ->
            R.encodeRoute $ R.R @() @'Ext.Html $ "-" :| "query" : undefined -- TODO
        )

encodeTagIndexR :: TagIndexR -> R.R 'Ext.Html
encodeTagIndexR (TagIndexR tagNodes) =
  R.R $ "-" :| "tags" : fmap (fromString . toString . HT.unTagNode) tagNodes
