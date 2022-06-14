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

    -- * Ema stuff
    emanoteRouteEncoder,
    emanoteGeneratableRoutes,
  )
where

import Data.IxSet.Typed qualified as Ix
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Time.Format (defaultTimeLocale, formatTime)
import Ema (UrlStrategy (..), routeUrlWith)
import Ema.Route.Encoder (RouteEncoder, mkRouteEncoder)
import Emanote.Model qualified as M
import Emanote.Model.Link.Rel qualified as Rel
import Emanote.Model.Meta qualified as Model
import Emanote.Model.Note qualified as N
import Emanote.Model.StaticFile qualified as SF
import Emanote.Model.Type (Model)
import Emanote.Pandoc.Markdown.Syntax.HashTag qualified as HT
import Emanote.Route qualified as R
import Emanote.Route.ModelRoute (LMLRoute, StaticFileRoute)
import Emanote.Route.SiteRoute.Type
import Emanote.View.LiveServerFiles qualified as LiveServerFile
import Optics.Core (prism')
import Optics.Operators ((^.))
import Relude

type EmanoteRouteEncoder = RouteEncoder Model SiteRoute

emanoteGeneratableRoutes :: Model -> [SiteRoute]
emanoteGeneratableRoutes model =
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
         in VirtualRoute_IndexR :
            VirtualRoute_ExportR :
            VirtualRoute_TasksR :
            (VirtualRoute_TagIndexR <$> toList tagPaths)
   in htmlRoutes
        <> staticRoutes
        <> fmap SiteRoute_VirtualRoute virtualRoutes

emanoteRouteEncoder :: EmanoteRouteEncoder
emanoteRouteEncoder =
  mkRouteEncoder $ \m -> prism' (enc m) (dec m)
  where
    enc model = \case
      SiteRoute_MissingR s ->
        -- error $ toText $ "emanote: attempt to encode a 404 route: " <> s
        -- Unfortunately, since ema:multisite does isomorphism check of
        -- encoder, we can't just error out here.
        s
      SiteRoute_AmbiguousR _ _ ->
        -- FIXME: See note above.
        error "emanote: attempt to encode an ambiguous route"
      SiteRoute_ResourceRoute r ->
        encodeResourceRoute model r
      SiteRoute_VirtualRoute r ->
        encodeVirtualRoute r

    dec model fp =
      fmap SiteRoute_VirtualRoute (decodeVirtualRoute fp)
        <|> decodeGeneratedRoute model fp
        <|> pure (SiteRoute_MissingR fp)

encodeResourceRoute :: HasCallStack => Model -> ResourceRoute -> FilePath
encodeResourceRoute model = \case
  ResourceRoute_LML r ->
    R.encodeRoute $
      -- HACK: This should never fail ... but *if* it does, consult
      -- https://github.com/srid/emanote/issues/148
      maybe
        -- FIXME: See note above.
        (error "emanote: attempt to encode missing note")
        N.noteHtmlRoute
        $ M.modelLookupNoteByRoute r model
  ResourceRoute_StaticFile r _fpAbs ->
    R.encodeRoute r

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
      SiteRoute_AmbiguousR ("/" <> fp) (N._noteRoute <$> ns)

noteFileSiteRoute :: N.Note -> SiteRoute
noteFileSiteRoute =
  lmlSiteRoute . N._noteRoute

lmlSiteRoute :: LMLRoute -> SiteRoute
lmlSiteRoute =
  SiteRoute_ResourceRoute . lmlResourceRoute

lmlResourceRoute :: LMLRoute -> ResourceRoute
lmlResourceRoute = ResourceRoute_LML

staticFileSiteRoute :: SF.StaticFile -> SiteRoute
staticFileSiteRoute =
  (SiteRoute_ResourceRoute . staticResourceRoute) . (SF._staticFileRoute &&& SF._staticFilePath)
  where
    staticResourceRoute :: (StaticFileRoute, FilePath) -> ResourceRoute
    staticResourceRoute = uncurry ResourceRoute_StaticFile

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
    staticFileRouteCase = \case
      SiteRoute_MissingR _fp ->
        Nothing
      SiteRoute_AmbiguousR _ _ ->
        Nothing
      SiteRoute_ResourceRoute rr ->
        case rr of
          ResourceRoute_StaticFile sfR _fp ->
            Just sfR
          ResourceRoute_LML _ ->
            Nothing
      SiteRoute_VirtualRoute _ -> Nothing

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
  SiteRoute_VirtualRoute VirtualRoute_IndexR

tagIndexRoute :: [HT.TagNode] -> SiteRoute
tagIndexRoute =
  SiteRoute_VirtualRoute . VirtualRoute_TagIndexR

taskIndexRoute :: SiteRoute
taskIndexRoute =
  SiteRoute_VirtualRoute VirtualRoute_TasksR
