{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Emanote.Route.SiteRoute.Class
  ( decodeVirtualRoute,
    noteFileSiteRoute,
    staticFileSiteRoute,
    lmlSiteRoute,
    indexRoute,
    tagIndexRoute,
    siteRouteUrl,
    urlStrategySuffix,
  )
where

import Control.Lens.Operators ((^.))
import qualified Data.IxSet.Typed as Ix
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.WorldPeace.Union
  ( absurdUnion,
    openUnionLift,
  )
import Ema (Ema (..), UrlStrategy (UrlDirect, UrlPretty), routeUrlWith)
import qualified Emanote.Model as M
import qualified Emanote.Model.Link.Rel as Rel
import qualified Emanote.Model.Meta as Model
import qualified Emanote.Model.Note as N
import qualified Emanote.Model.StaticFile as SF
import Emanote.Model.Type (Model)
import qualified Emanote.Pandoc.Markdown.Syntax.HashTag as HT
import Emanote.Prelude (h)
import qualified Emanote.Route as R
import Emanote.Route.ModelRoute (LMLRoute, StaticFileRoute)
import Emanote.Route.SiteRoute.Type
import qualified Emanote.View.LiveServerFiles as LiveServerFile

instance Ema Model SiteRoute where
  encodeRoute :: HasCallStack => Model -> SiteRoute -> FilePath
  encodeRoute model (SiteRoute r) =
    r
      & absurdUnion
      `h` ( \(MissingR _fp) ->
              error "emanote: attempt to encode a 404 route"
          )
      `h` ( \(AmbiguousR _) ->
              error "emanote: attempt to encode an ambiguous route"
          )
      `h` encodeResourceRoute model
      `h` encodeVirtualRoute

  decodeRoute model fp =
    fmap (SiteRoute . openUnionLift) (decodeVirtualRoute fp)
      <|> decodeGeneratedRoute model fp
      <|> pure (SiteRoute $ openUnionLift $ MissingR fp)

  allRoutes model =
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
              (openUnionLift . TagIndexR <$> toList tagPaths)
     in htmlRoutes
          <> staticRoutes
          <> fmap (SiteRoute . openUnionLift) virtualRoutes

encodeResourceRoute :: HasCallStack => Model -> ResourceRoute -> FilePath
encodeResourceRoute model =
  absurdUnion
    `h` ( \(r :: LMLRoute) ->
            R.encodeRoute $
              -- HACK: This should never fail.
              maybe (error $ "attempt to encode a non-existance note route: " <> show r) N.noteHtmlRoute $
                M.modelLookupNoteByRoute r model
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

siteRouteUrl :: HasCallStack => Model -> SiteRoute -> Text
siteRouteUrl model =
  Ema.routeUrlWith (urlStrategy model) model

urlStrategySuffix :: Model -> Text
urlStrategySuffix model =
  case urlStrategy model of
    Ema.UrlDirect -> ".html"
    Ema.UrlPretty -> ""

urlStrategy :: Model -> UrlStrategy
urlStrategy =
  Model.lookupRouteMeta Ema.UrlDirect ("template" :| one "urlStrategy") indexLmlRoute
  where
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
