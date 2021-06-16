module Emanote.Pandoc.Filter.Url (urlResolvingSplice) where

import Control.Lens.Operators ((^.))
import Control.Monad.Except (throwError)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.WorldPeace.Union (absurdUnion, openUnionLift)
import qualified Ema
import qualified Ema.CLI
import Emanote.Model (Model)
import qualified Emanote.Model as M
import qualified Emanote.Model.Link.Rel as Rel
import qualified Emanote.Model.Note as MN
import qualified Emanote.Model.StaticFile as SF
import Emanote.Prelude (h)
import qualified Emanote.Route as R
import qualified Emanote.Route.SiteRoute as SR
import qualified Heist.Extra.Splices.Pandoc as HP
import Heist.Extra.Splices.Pandoc.Ctx (ctxSansCustomSplicing)
import Heist.Extra.Splices.Pandoc.Render (plainify)
import qualified Heist.Interpreted as HI
import qualified Text.Pandoc.Definition as B

-- | Resolve all URLs in inlines (<a> and <img>)
urlResolvingSplice ::
  Monad n => Ema.CLI.Action -> Model -> HP.RenderCtx n -> B.Inline -> Maybe (HI.Splice n)
urlResolvingSplice emaAction model (ctxSansCustomSplicing -> ctx) inl =
  case inl of
    B.Link attr@(_id, _class, otherAttrs) is (url, tit) ->
      pure $ case resolveUrl emaAction model (otherAttrs <> one ("title", tit)) (is, url) of
        Left err ->
          brokenLinkSpanWrapper err inl
        Right (newIs, newUrl) ->
          HP.rpInline ctx $ B.Link attr newIs (newUrl, tit)
    B.Image attr@(id', class', otherAttrs) is' (url, tit) -> do
      let is = imageInlineFallback url is'
      pure $ case resolveUrl emaAction model (otherAttrs <> one ("title", tit)) (is, url) of
        Left err ->
          brokenLinkSpanWrapper err $
            B.Image (id', class', otherAttrs) is (url, tit)
        Right (newIs, newUrl) ->
          HP.rpInline ctx $ B.Image attr newIs (newUrl, tit)
    _ -> Nothing
  where
    -- Fallback to filename if no "alt" text is specified
    imageInlineFallback fn = \case
      [] -> one $ B.Str fn
      x -> x
    brokenLinkSpanWrapper err inline =
      HP.rpInline ctx $
        B.Span ("", one "emanote:broken-link", one ("title", err)) $
          one inline

resolveUrl :: Ema.CLI.Action -> Model -> [(Text, Text)] -> ([B.Inline], Text) -> Either Text ([B.Inline], Text)
resolveUrl emaAction model linkAttrs x@(inner, url) =
  fromMaybe (Right x) $ do
    uRel <- Rel.parseUnresolvedRelTarget linkAttrs url
    let target = resolveUnresolvedRelTarget model uRel
    pure $ second renderUrl target
  where
    renderUrl ::
      (SR.SiteRoute, Maybe UTCTime) ->
      ([B.Inline], Text)
    renderUrl (r, mTime) =
      let isWikiLinkSansCustom = url == plainify inner
          mQuery = do
            -- In live server mode, append last modification time if any, such
            -- that the browser is forced to refresh the inline image on hot
            -- reload (Ema's DOM patch).
            guard $ emaAction == Ema.CLI.Run
            t <- mTime
            pure $ toText $ "?t=" <> formatTime defaultTimeLocale "%s" t
       in ( fromMaybe
              inner
              ( guard isWikiLinkSansCustom >> wikiLinkDefaultInnerText r
              ),
            Ema.routeUrl model r <> fromMaybe "" mQuery
          )
      where
        wikiLinkDefaultInnerText =
          absurdUnion
            `h` (\(SR.MissingR _) -> Nothing)
            `h` ( \(resR :: SR.ResourceRoute) ->
                    resR & absurdUnion
                      `h` ( \(lmlR :: R.LMLRoute) ->
                              one . B.Str . MN.noteTitle <$> M.modelLookupNoteByRoute lmlR model
                          )
                      `h` ( \(_ :: R.StaticFileRoute, _ :: FilePath) ->
                              -- Just append a file: prefix.
                              pure $ B.Str "File: " : inner
                          )
                )
            `h` (\(_ :: SR.VirtualRoute) -> Nothing)

resolveUnresolvedRelTarget ::
  Model -> Rel.UnresolvedRelTarget -> Either Text (SR.SiteRoute, Maybe UTCTime)
resolveUnresolvedRelTarget model = \case
  Rel.URTResource r ->
    resolveModelRouteMustExist r
  Rel.URTWikiLink (_wlType, wl) -> do
    resourceSiteRoute <$> resolveWikiLinkMustExist wl
  where
    resolveWikiLinkMustExist wl =
      case nonEmpty (M.modelWikiLinkTargets wl model) of
        Nothing -> do
          throwError "Wiki-link does not refer to any known file"
        Just (target :| []) ->
          pure target
        Just targets -> do
          let targetsStr =
                targets <&> \case
                  Left note -> R.encodeRoute $ R.lmlRouteCase $ note ^. MN.noteRoute
                  Right sf -> R.encodeRoute $ sf ^. SF.staticFileRoute
          throwError $
            "Wikilink "
              <> show wl
              <> " is ambiguous; referring to one of: "
              <> T.intercalate ", " (toText <$> toList targetsStr)
    resolveModelRouteMustExist r =
      case resolveModelRoute model r of
        Nothing ->
          Left "Link does not refer to any known file"
        Just v -> Right v

resolveModelRoute :: Model -> R.ModelRoute -> Maybe (SR.SiteRoute, Maybe UTCTime)
resolveModelRoute model lr = do
  let eRoute = R.modelRouteCase lr
  let meRes =
        bitraverse
          (`M.modelLookupNoteByRoute` model)
          (`M.modelLookupStaticFileByRoute` model)
          eRoute
  case meRes of
    Just eRes ->
      -- The route resolves to something in the model
      pure $ resourceSiteRoute eRes
    Nothing -> do
      -- The route does not resolve to anything in the model
      -- If this route is a AnyExt, let's decoding it as a "non resource" route.
      -- This HACK should go away upon refactoring SiteRoute /
      -- UnresolvedRelTarget types (see their comments).
      case eRoute of
        Left _ -> Nothing
        Right (R.encodeRoute -> rawPath) -> do
          SR.decodeVirtualRoute rawPath
            <&> (,Nothing) . openUnionLift

resourceSiteRoute :: Either MN.Note SF.StaticFile -> (SR.SiteRoute, Maybe UTCTime)
resourceSiteRoute = \case
  Left note ->
    SR.noteFileSiteRoute note
      & (,Nothing)
  Right sf ->
    SR.staticFileSiteRoute sf
      & (,Just $ sf ^. SF.staticFileTime)
