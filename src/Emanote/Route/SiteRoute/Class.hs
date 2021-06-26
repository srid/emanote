{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Emanote.Route.SiteRoute.Class
  ( decodeVirtualRoute,
    noteFileSiteRoute,
    staticFileSiteRoute,
    lmlSiteRoute,
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
import Ema (Ema (..))
import qualified Emanote.Model as M
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
  encodeRoute model =
    absurdUnion
      `h` ( \(MissingR _fp) ->
              error "emanote: attempt to encode a 404 route"
          )
      `h` encodeResourceRoute model
      `h` encodeVirtualRoute

  decodeRoute model fp =
    fmap openUnionLift (decodeVirtualRoute fp)
      <|> decodeGeneratedRoute model fp
      <|> pure (openUnionLift $ MissingR fp)

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
                  concat $
                    tags <&> \(HT.deconstructTag -> tagPath) ->
                      toList $ NE.inits tagPath
           in openUnionLift IndexR :
              (openUnionLift . TagIndexR <$> toList tagPaths)
     in htmlRoutes
          <> staticRoutes
          <> fmap openUnionLift virtualRoutes

encodeResourceRoute :: Model -> ResourceRoute -> FilePath
encodeResourceRoute model =
  absurdUnion
    `h` ( \(r :: LMLRoute) ->
            R.encodeRoute $
              maybe (coerce . R.lmlRouteCase $ r) N.noteHtmlRoute $
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
    <|> fmap
      noteFileSiteRoute
      (flip M.modelLookupNoteByHtmlRoute model $ R.decodeHtmlRoute fp)

noteFileSiteRoute :: N.Note -> SiteRoute
noteFileSiteRoute =
  lmlSiteRoute . N._noteRoute

lmlSiteRoute :: LMLRoute -> SiteRoute
lmlSiteRoute =
  openUnionLift . lmlResourceRoute

lmlResourceRoute :: LMLRoute -> ResourceRoute
lmlResourceRoute =
  openUnionLift

staticFileSiteRoute :: SF.StaticFile -> SiteRoute
staticFileSiteRoute =
  (openUnionLift . staticResourceRoute) . (SF._staticFileRoute &&& SF._staticFilePath)
  where
    staticResourceRoute :: (StaticFileRoute, FilePath) -> ResourceRoute
    staticResourceRoute =
      openUnionLift
