module Emanote.Route.SiteRoute.Class (
  decodeVirtualRoute,
  noteFileSiteRoute,
  noteFeedSiteRoute,
  staticFileSiteRoute,
  lmlSiteRoute,
  indexRoute,
  tagIndexRoute,
  taskIndexRoute,
  siteRouteUrl,
  siteRouteUrlStatic,
  urlStrategySuffix,

  -- * Ema stuff
  emanoteRouteEncoder,
  emanoteGeneratableRoutes,
) where

import Data.IxSet.Typed qualified as Ix
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Time.Format (defaultTimeLocale, formatTime)
import Ema (UrlStrategy (..), routeUrlWith)
import Emanote.Model qualified as M
import Emanote.Model.Link.Rel qualified as Rel
import Emanote.Model.Meta qualified as Model
import Emanote.Model.Note qualified as N
import Emanote.Model.StaticFile qualified as SF
import Emanote.Model.Type (Model, ModelEma, ModelT)
import Emanote.Pandoc.Markdown.Syntax.HashTag qualified as HT
import Emanote.Route qualified as R
import Emanote.Route.ModelRoute (LMLRoute, StaticFileRoute)
import Emanote.Route.SiteRoute.Type
import Emanote.View.LiveServerFiles qualified as LiveServerFile
import Optics.Core (Prism', prism')
import Optics.Operators ((^.))
import Relude

emanoteGeneratableRoutes :: ModelEma -> [SiteRoute]
emanoteGeneratableRoutes model =
  let htmlRoutes =
        model ^. M.modelNotes
          & Ix.toList
          <&> noteFileSiteRoute
      feedRoutes =
        model ^. M.modelNotes
          & Ix.toList
          & filter N.noteHasFeed
          <&> noteFeedSiteRoute
      staticRoutes =
        let includeFile f =
              not (LiveServerFile.isLiveServerFile f)
                || (f == LiveServerFile.tailwindFullCssPath && not (model ^. M.modelCompileTailwind))
         in model ^. M.modelStaticFiles
              & Ix.toList
              & filter (includeFile . R.encodeRoute . SF._staticFileRoute)
              <&> staticFileSiteRoute
      virtualRoutes :: [VirtualRoute] =
        let tags = fst <$> M.modelTags model
            tagPaths =
              Set.fromList $
                ([] :) $ -- [] Triggers generation of main tag index.
                  concat $
                    tags <&> \(HT.deconstructTag -> tagPath) ->
                      NE.filter (not . null) $ NE.inits tagPath
         in VirtualRoute_Index
              : VirtualRoute_Export
              : VirtualRoute_StorkIndex
              : VirtualRoute_TaskIndex
              : (VirtualRoute_TagIndex <$> toList tagPaths)
   in htmlRoutes
        <> feedRoutes
        <> staticRoutes
        <> fmap SiteRoute_VirtualRoute virtualRoutes

emanoteRouteEncoder :: HasCallStack => ModelEma -> Prism' FilePath SiteRoute
emanoteRouteEncoder model =
  prism' enc dec
  where
    enc = \case
      SiteRoute_MissingR s ->
        -- error $ toText $ "emanote: attempt to encode a 404 route: " <> s
        -- Unfortunately, since ema:multisite does isomorphism check of
        -- encoder, we can't just error out here.
        s
      SiteRoute_AmbiguousR fp _ ->
        -- FIXME: See note above.
        error $ "emanote: attempt to encode an ambiguous route: " <> toText fp
      SiteRoute_ResourceRoute r ->
        encodeResourceRoute model r
      SiteRoute_VirtualRoute r ->
        encodeVirtualRoute r

    dec fp =
      fmap SiteRoute_VirtualRoute (decodeVirtualRoute fp)
        <|> decodeGeneratedRoute model fp
        <|> pure (SiteRoute_MissingR fp)

encodeResourceRoute :: HasCallStack => ModelEma -> ResourceRoute -> FilePath
encodeResourceRoute model = \case
  ResourceRoute_LML LMLView_Html r ->
    R.encodeRoute
      $
      -- HACK: This should never fail ... but *if* it does, consult
      -- https://github.com/srid/emanote/issues/148
      maybe
        -- FIXME: See note above.
        (error $ "emanote: attempt to encode missing note: " <> show r)
        N.noteHtmlRoute
      $ M.modelLookupNoteByRoute r model
  ResourceRoute_LML LMLView_Atom r ->
    R.encodeRoute
      $ fromMaybe
        -- FIXME: See note above.
        (error $ "emanote: attempt to encode missing feed: " <> show r)
      $ N.noteXmlRoute =<< M.modelLookupNoteByRoute r model
  ResourceRoute_StaticFile r _fpAbs ->
    R.encodeRoute r

-- | Decode a route that is known to refer to a resource in the model
decodeGeneratedRoute :: ModelEma -> FilePath -> Maybe SiteRoute
decodeGeneratedRoute model fp =
  fmap
    staticFileSiteRoute
    (flip M.modelLookupStaticFileByRoute model =<< R.decodeAnyRoute fp)
    <|> mFeedRoute
    <|> noteHtmlSiteRoute
      (flip M.modelLookupNoteByHtmlRoute model $ R.decodeHtmlRoute fp)
  where
    mFeedRoute :: Maybe SiteRoute
    mFeedRoute = case R.decodeXmlRoute fp of
      Nothing -> Nothing
      Just r -> noteFeedSiteRoute <$> M.modelLookupFeedNoteByHtmlRoute r model
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

noteFeedSiteRoute :: N.Note -> SiteRoute
noteFeedSiteRoute = SiteRoute_ResourceRoute . ResourceRoute_LML LMLView_Atom . N._noteRoute

noteFileSiteRoute :: N.Note -> SiteRoute
noteFileSiteRoute =
  lmlSiteRoute . N._noteRoute

lmlSiteRoute :: LMLRoute -> SiteRoute
lmlSiteRoute =
  SiteRoute_ResourceRoute . lmlResourceRoute

lmlResourceRoute :: LMLRoute -> ResourceRoute
lmlResourceRoute = ResourceRoute_LML LMLView_Html

staticFileSiteRoute :: SF.StaticFile -> SiteRoute
staticFileSiteRoute =
  (SiteRoute_ResourceRoute . staticResourceRoute) . (SF._staticFileRoute &&& SF._staticFilePath)
  where
    staticResourceRoute :: (StaticFileRoute, FilePath) -> ResourceRoute
    staticResourceRoute = uncurry ResourceRoute_StaticFile

-- | Like `siteRouteUrl` but avoids any dynamism in the URL
siteRouteUrlStatic :: HasCallStack => Model -> SiteRoute -> Text
siteRouteUrlStatic model =
  Ema.routeUrlWith (urlStrategy model) rp
  where
    (rp, _) = M.withoutRoutePrism model

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
          ResourceRoute_LML _ _ ->
            Nothing
      SiteRoute_VirtualRoute _ -> Nothing

urlStrategySuffix :: Model -> Text
urlStrategySuffix model =
  case urlStrategy model of
    Ema.UrlDirect -> ".html"
    Ema.UrlPretty -> ""

urlStrategy :: ModelT f -> UrlStrategy
urlStrategy model =
  Model.lookupRouteMeta Ema.UrlDirect ("template" :| one "urlStrategy") (M.modelIndexRoute model) model

indexRoute :: SiteRoute
indexRoute =
  SiteRoute_VirtualRoute VirtualRoute_Index

tagIndexRoute :: [HT.TagNode] -> SiteRoute
tagIndexRoute =
  SiteRoute_VirtualRoute . VirtualRoute_TagIndex

taskIndexRoute :: SiteRoute
taskIndexRoute =
  SiteRoute_VirtualRoute VirtualRoute_TaskIndex
