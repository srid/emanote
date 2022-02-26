{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Emanote.Route.SiteRoute.Class
  ( decodeVirtualRoute,
    noteFileSiteRoute,
    staticFileSiteRoute,
    lmlSiteRoute,
    indexRoute,
    indexLmlRoute,
    tagIndexRoute,
    taskIndexRoute,
    siteRouteUrl,
    siteRouteUrlStatic,
    urlStrategySuffix,
    routeEncoder,
  )
where

import Control.Lens.Operators ((^.))
import Data.IxSet.Typed qualified as Ix
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.WorldPeace.Union
  ( absurdUnion,
    openUnionLift,
  )
import Ema (UrlStrategy (UrlDirect, UrlPretty), routeUrlWith)
import Ema.Route
import Emanote.Model qualified as M
import Emanote.Model.Link.Rel qualified as Rel
import Emanote.Model.Meta qualified as Model
import Emanote.Model.Note qualified as N
import Emanote.Model.StaticFile qualified as SF
import Emanote.Model.Type (Model)
import Emanote.Pandoc.Markdown.Syntax.HashTag qualified as HT
import Emanote.Prelude (h)
import Emanote.Route qualified as R
import Emanote.Route.ModelRoute (LMLRoute, StaticFileRoute)
import Emanote.Route.SiteRoute.Type
import Emanote.View.LiveServerFiles qualified as LiveServerFile
import Relude

type EmanoteRouteEncoder = RouteEncoder Model SiteRoute

routeEncoder :: EmanoteRouteEncoder
routeEncoder =
  unsafeMkRouteEncoder enc dec all_
  where
    enc model (SiteRoute r) =
      r
        & absurdUnion
        `h` ( \(MissingR s) ->
                error $ toText $ "emanote: attempt to encode a 404 route: " <> s
            )
        `h` ( \(AmbiguousR _) ->
                error "emanote: attempt to encode an ambiguous route"
            )
        `h` encodeResourceRoute model
        `h` encodeVirtualRoute

    dec model fp =
      fmap (SiteRoute . openUnionLift) (decodeVirtualRoute fp)
        <|> decodeGeneratedRoute model fp
        <|> pure (SiteRoute $ openUnionLift $ MissingR fp)

    -- Only these routes will be generated in static-site generation mode.
    all_ model =
      let htmlRoutes =
            model ^. M.modelNotes
              & Ix.toList
              <&> noteFileSiteRoute
          staticRoutes =
            model ^. M.modelStaticFiles
              & Ix.toList
              & filter (not . LiveServerFile.isLiveServerFile . R.encodeRoute . SF._staticFileRoute)
              <&> staticFileSiteRoute
          virtualRoutes :: [VirtualRoute] =
            let tags = fst <$> M.modelTags model
                tagPaths =
                  Set.fromList $
                    ([] :) $ -- [] Triggers generation of main tag index.
                      concat $
                        tags <&> \(HT.deconstructTag -> tagPath) ->
                          NE.filter (not . null) $ NE.inits tagPath
             in openUnionLift IndexR :
                openUnionLift ExportR :
                openUnionLift TasksR :
                (openUnionLift . TagIndexR <$> toList tagPaths)
       in htmlRoutes
            <> staticRoutes
            <> fmap (SiteRoute . openUnionLift) virtualRoutes

encodeResourceRoute :: HasCallStack => Model -> ResourceRoute -> FilePath
encodeResourceRoute model =
  absurdUnion
    `h` ( \(r :: LMLRoute) ->
            R.encodeRoute $
              -- HACK: This should never fail ... but *if* it does, consult
              -- https://github.com/srid/emanote/issues/148
              maybe
                (error "emanote: attempt to encode missing note")
                N.noteHtmlRoute
                $ M.modelLookupNoteByRoute r model
        )
    `h` ( \(r :: StaticFileRoute, _fpAbs :: FilePath) ->
            R.encodeRoute r
        )

-- | Decode a route that is known to refer to a resource in the model
decodeGeneratedRoute :: Model -> FilePath -> Maybe SiteRoute
decodeGeneratedRoute model fp =
  fmap
    staticFileSiteRoute
    (flip M.modelLookupStaticFileByRoute model =<< R.decodeAnyRoute fp)
    <|> noteHtmlSiteRoute
      (flip M.modelLookupNoteByHtmlRoute model $ R.decodeHtmlRoute fp)
  where
    noteHtmlSiteRoute :: Rel.ResolvedRelTarget N.Note -> Maybe SiteRoute
    noteHtmlSiteRoute = \case
      Rel.RRTMissing ->
        Nothing
      Rel.RRTFound note ->
        Just $ noteFileSiteRoute note
      Rel.RRTAmbiguous notes ->
        Just $ ambiguousNoteURLsRoute notes
    ambiguousNoteURLsRoute :: NonEmpty N.Note -> SiteRoute
    ambiguousNoteURLsRoute ns =
      SiteRoute $ openUnionLift $ AmbiguousR ("/" <> fp, N._noteRoute <$> ns)

noteFileSiteRoute :: N.Note -> SiteRoute
noteFileSiteRoute =
  lmlSiteRoute . N._noteRoute

lmlSiteRoute :: LMLRoute -> SiteRoute
lmlSiteRoute =
  SiteRoute . openUnionLift . lmlResourceRoute

lmlResourceRoute :: LMLRoute -> ResourceRoute
lmlResourceRoute =
  openUnionLift

staticFileSiteRoute :: SF.StaticFile -> SiteRoute
staticFileSiteRoute =
  (SiteRoute . openUnionLift . staticResourceRoute) . (SF._staticFileRoute &&& SF._staticFilePath)
  where
    staticResourceRoute :: (StaticFileRoute, FilePath) -> ResourceRoute
    staticResourceRoute =
      openUnionLift

-- | Like `siteRouteUrl` but avoids any dynamism in the URL
siteRouteUrlStatic :: HasCallStack => Model -> SiteRoute -> Text
siteRouteUrlStatic model =
  Ema.routeUrlWith (urlStrategy model) (model ^. M.modelRouteEncoder) model

siteRouteUrl :: HasCallStack => Model -> SiteRoute -> Text
siteRouteUrl model sr =
  siteRouteUrlStatic model sr
    <> siteRouteQuery
  where
    siteRouteQuery =
      maybe "" (("?t=" <>) . toText . formatTime defaultTimeLocale "%s") staticFileModifiedTime
    staticFileModifiedTime = do
      -- In live server model, we append a ?t=.. to trigger the browser into
      -- reloading (or invalidating its cache of) this embed static file.
      guard $ M.inLiveServer model
      sfRoute <- staticFileRouteCase sr
      sf <- M.modelLookupStaticFileByRoute sfRoute model
      pure $ sf ^. SF.staticFileTime
    staticFileRouteCase :: SiteRoute -> Maybe StaticFileRoute
    staticFileRouteCase (SiteRoute r) =
      r & absurdUnion
        `h` ( \(MissingR _fp) ->
                Nothing
            )
        `h` ( \(AmbiguousR _) ->
                Nothing
            )
        `h` ( \(rr :: ResourceRoute) ->
                rr & absurdUnion
                  `h` ( \(sfR :: StaticFileRoute, _fp :: FilePath) ->
                          Just sfR
                      )
                  `h` ( \(_ :: LMLRoute) ->
                          Nothing
                      )
            )
        `h` (\(_ :: VirtualRoute) -> Nothing)

urlStrategySuffix :: Model -> Text
urlStrategySuffix model =
  case urlStrategy model of
    Ema.UrlDirect -> ".html"
    Ema.UrlPretty -> ""

urlStrategy :: Model -> UrlStrategy
urlStrategy =
  Model.lookupRouteMeta Ema.UrlDirect ("template" :| one "urlStrategy") indexLmlRoute

indexLmlRoute :: LMLRoute
indexLmlRoute =
  R.liftLMLRoute @('R.LMLType 'R.Md) $ R.indexRoute

indexRoute :: SiteRoute
indexRoute =
  let virtR :: VirtualRoute = openUnionLift IndexR
   in SiteRoute $ openUnionLift virtR

tagIndexRoute :: [HT.TagNode] -> SiteRoute
tagIndexRoute (TagIndexR -> tagR) =
  let virtR :: VirtualRoute = openUnionLift tagR
   in SiteRoute $ openUnionLift virtR

taskIndexRoute :: SiteRoute
taskIndexRoute =
  let virtR :: VirtualRoute = openUnionLift TasksR
   in SiteRoute $ openUnionLift virtR
