module Emanote.Pandoc.Renderer.Url
  ( urlResolvingSplice,
    plainifyWikiLinkSplice,
  )
where

import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.WorldPeace.Union (absurdUnion)
import qualified Ema.CLI
import Emanote.Model (Model)
import qualified Emanote.Model as M
import qualified Emanote.Model.Link.Rel as Rel
import qualified Emanote.Model.Link.Resolve as Resolve
import qualified Emanote.Model.Note as MN
import qualified Emanote.Model.Title as Tit
import qualified Emanote.Pandoc.Markdown.Syntax.WikiLink as WL
import Emanote.Pandoc.Renderer (PandocInlineRenderer)
import Emanote.Pandoc.Renderer.BrokenLink
  ( BrokenLink (BrokenLink_Inline),
    nonEmptyInlines,
    renderBrokenLink,
  )
import Emanote.Prelude (h)
import qualified Emanote.Route as R
import qualified Emanote.Route.SiteRoute as SR
import qualified Heist.Extra.Splices.Pandoc as HP
import Heist.Extra.Splices.Pandoc.Ctx (ctxSansCustomSplicing)
import qualified Text.Pandoc.Definition as B
import qualified Text.Pandoc.Walk as W

-- | Resolve all URLs in inlines (<a> and <img>)
urlResolvingSplice :: Monad n => PandocInlineRenderer n i b
urlResolvingSplice emaAction model _nf (ctxSansCustomSplicing -> ctx) _ inl =
  case inl of
    B.Link attr@(_id, _class, otherAttrs) is (url, tit) -> do
      uRel <- Rel.parseUnresolvedRelTarget (otherAttrs <> one ("title", tit)) url
      case Resolve.resolveUnresolvedRelTarget model uRel of
        Left err -> do
          let brokenLink = BrokenLink_Inline attr is (url, tit)
          pure $ renderBrokenLink ctx err brokenLink
        Right sr -> do
          -- TODO: If uRel is `Rel.URTWikiLink (WL.WikiLinkEmbed, _)`, *and* it appears
          -- in B.Para (so do this in block-level custom splice), then embed it.
          -- We don't do this here, as this inline splice can't embed block elements.
          let (newIs, newUrl) = replaceLinkNodeWithRoute emaAction model sr (is, url)
          pure $ HP.rpInline ctx $ B.Link attr newIs (newUrl, tit)
    B.Image attr@(_, _, otherAttrs) is (url, tit) -> do
      uRel <- Rel.parseUnresolvedRelTarget (otherAttrs <> one ("title", tit)) url
      case Resolve.resolveUnresolvedRelTarget model uRel of
        Left err -> do
          let brokenLink = BrokenLink_Inline attr is (url, tit)
          pure $ renderBrokenLink ctx err brokenLink
        Right sr -> do
          let (newIs, newUrl) =
                replaceLinkNodeWithRoute emaAction model sr (toList $ nonEmptyInlines url is, url)
          pure $ HP.rpInline ctx $ B.Image attr newIs (newUrl, tit)
    _ -> Nothing

plainifyWikiLinkSplice :: Monad n => PandocInlineRenderer n i b
plainifyWikiLinkSplice _emaAction _model _nf (ctxSansCustomSplicing -> ctx) _ inl = do
  wl <- WL.inlineToWikiLink inl
  pure $ HP.rpInline ctx $ B.Str $ show wl

inlinesWithWikiLinksPlainified :: [B.Inline] -> [B.Inline]
inlinesWithWikiLinksPlainified = W.walk $ \case
  (WL.inlineToWikiLink -> Just wl) ->
    B.Str (show wl)
  x -> x

replaceLinkNodeWithRoute ::
  Ema.CLI.Action ->
  Model ->
  (SR.SiteRoute, Maybe UTCTime) ->
  ([B.Inline], Text) ->
  ([B.Inline], Text)
replaceLinkNodeWithRoute emaAction model (r, mTime) (inner, url) =
  ( inlinesWithWikiLinksPlainified $ nonEmptyLinkInlines model url (Just r) inner,
    foldUrlTime (SR.siteRouteUrl model r) mTime
  )
  where
    foldUrlTime linkUrl mUrlTime =
      linkUrl
        -- In live server mode, append last modification time if any, such
        -- that the browser is forced to refresh the inline image on hot
        -- reload (Ema's DOM patch).
        <> fromMaybe
          ""
          ( do
              guard $ emaAction == Ema.CLI.Run
              t <- mUrlTime
              pure $ toText $ "?t=" <> formatTime defaultTimeLocale "%s" t
          )

nonEmptyLinkInlines :: Model -> Text -> Maybe SR.SiteRoute -> [B.Inline] -> [B.Inline]
nonEmptyLinkInlines model url mr = \case
  [] ->
    toList $
      nonEmptyInlines url $
        fromMaybe [] $
          siteRouteDefaultInnerText model url =<< mr
  x -> x

siteRouteDefaultInnerText :: Model -> Text -> SR.SiteRoute -> Maybe [B.Inline]
siteRouteDefaultInnerText model url =
  absurdUnion
    `h` (\(SR.MissingR _) -> Nothing)
    `h` (\(SR.AmbiguousR _) -> Nothing)
    `h` ( \(resR :: SR.ResourceRoute) ->
            resR & absurdUnion
              `h` ( \(lmlR :: R.LMLRoute) ->
                      Tit.toInlines . MN._noteTitle <$> M.modelLookupNoteByRoute lmlR model
                  )
              `h` ( \(_ :: R.StaticFileRoute, _ :: FilePath) ->
                      -- Just append a file: prefix, to existing wiki-link.
                      pure $ B.Str "File:" : [B.Str url]
                  )
        )
    `h` (\(_ :: SR.VirtualRoute) -> Nothing)
