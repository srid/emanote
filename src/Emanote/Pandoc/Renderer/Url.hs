{-# LANGUAGE TypeApplications #-}

module Emanote.Pandoc.Renderer.Url
  ( urlResolvingSplice,
    plainifyWikiLinkSplice,
    renderSomeInlineRefWith,
  )
where

import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.WorldPeace.Union (absurdUnion)
import qualified Ema
import qualified Ema.CLI
import Ema.Helper.Markdown (plainify)
import Emanote.Model (Model)
import qualified Emanote.Model as M
import qualified Emanote.Model.Link.Rel as Rel
import qualified Emanote.Model.Link.Resolve as Resolve
import qualified Emanote.Model.Note as MN
import qualified Emanote.Model.Title as Tit
import qualified Emanote.Pandoc.Markdown.Syntax.WikiLink as WL
import Emanote.Pandoc.Renderer (PandocInlineRenderer)
import Emanote.Prelude (h)
import qualified Emanote.Route as R
import qualified Emanote.Route.SiteRoute as SR
import qualified Heist.Extra.Splices.Pandoc as HP
import qualified Heist.Extra.Splices.Pandoc as Splices
import Heist.Extra.Splices.Pandoc.Ctx (ctxSansCustomSplicing)
import qualified Heist.Interpreted as HI
import qualified Text.Pandoc.Definition as B
import qualified Text.Pandoc.Walk as W

unParseLink :: HasCallStack => B.Inline -> Text
unParseLink inl =
  case WL.wikiLinkInlineRendered inl of
    Just url ->
      url
    Nothing ->
      -- TODO: reuse parseInlineRef?
      case inl of
        B.Link _ is (url, _tit) ->
          "[" <> plainify is <> "](" <> url <> ")"
        B.Image _ is (url, _tit) ->
          "![" <> plainify is <> "](" <> url <> ")"
        _ ->
          error "Non-link inline"

tooltip :: Text -> [B.Inline] -> B.Inline
tooltip s = B.Span ("", [], one ("title", s))

-- | A Pandoc inline that refers to something else.
data InlineRef
  = InlineLink
  | InlineImage
  deriving (Eq, Show)

parseInlineRef :: B.Inline -> Maybe (InlineRef, B.Attr, [B.Inline], (Text, Text))
parseInlineRef = \case
  B.Link attr is (url, tit) ->
    pure (InlineLink, attr, is, (url, tit))
  B.Image attr is (url, tit) ->
    pure (InlineImage, attr, is, (url, tit))
  _ ->
    Nothing

renderSomeInlineRefWith ::
  Monad n =>
  ( a ->
    Maybe (HI.Splice n)
  ) ->
  (a -> (SR.SiteRoute, Maybe UTCTime)) ->
  (B.Attr, [B.Inline], (Text, Text)) ->
  Rel.ResolvedRelTarget a ->
  Ema.CLI.Action ->
  Model ->
  Splices.RenderCtx n ->
  B.Inline ->
  Maybe (HI.Splice n)
renderSomeInlineRefWith f getSr (_, is, (url, tit)) rRel emaAction model (ctxSansCustomSplicing -> ctx) origInl = do
  case rRel of
    Rel.RRTMissing -> do
      pure $ do
        raw <-
          HP.rpInline
            ctx
            ( tooltip
                "Link is broken"
                [ B.Strikeout $ one $ B.Str $ unParseLink origInl,
                  B.Str "❌" -- TODO; add tooltip
                ]
            )
        lnk <-
          HP.rpInline ctx $
            B.Span ("", ["emanote:error:aside"], []) $
              one $
                tooltip "Find notes using this broken link" $
                  one $
                    B.Link B.nullAttr (one $ B.Str "Refs") (url, "")
        pure $ raw <> lnk
    Rel.RRTAmbiguous srs -> do
      pure $ do
        raw <- HP.rpInline ctx (tooltip "Link is ambiguous" [B.Strikeout $ one $ B.Str $ unParseLink origInl, B.Str "❗"]) -- TODO; add tooltip
        candids <-
          fmap mconcat . sequence $
            toList srs
              <&> \(getSr -> sr) -> do
                let srRoute = toText $ Ema.encodeRoute model (fst sr)
                    (_newIs, newUrl) = replaceLinkNodeWithRoute emaAction model sr (is, srRoute)
                    newIs = one $ B.Str $ show $ fst sr
                HP.rpInline ctx $
                  B.Span ("", ["emanote:error:aside"], []) $
                    one $
                      tooltip (show (fst sr) <> " -> " <> srRoute) $
                        one $
                          B.Link B.nullAttr newIs (newUrl, tit)
        pure $ raw <> candids
    Rel.RRTFound sr -> do
      f sr

-- | Resolve all URLs in inlines (<a> and <img>)
urlResolvingSplice :: Monad n => PandocInlineRenderer n i b
urlResolvingSplice emaAction model _nf (ctxSansCustomSplicing -> ctx) _ inl = do
  (inlRef, attr@(_, _, otherAttrs), is, (url, tit)) <- parseInlineRef inl
  let f = \sr -> do
        case inlRef of
          InlineLink -> do
            -- TODO: If uRel is `Rel.URTWikiLink (WL.WikiLinkEmbed, _)`, *and* it appears
            -- in B.Para (so do this in block-level custom splice), then embed it.
            -- We don't do this here, as this inline splice can't embed block elements.
            let (newIs, newUrl) = replaceLinkNodeWithRoute emaAction model sr (is, url)
            pure $ HP.rpInline ctx $ B.Link attr newIs (newUrl, tit)
          InlineImage -> do
            let (newIs, newUrl) =
                  replaceLinkNodeWithRoute emaAction model sr (toList $ nonEmptyInlines url is, url)
            pure $ HP.rpInline ctx $ B.Image attr newIs (newUrl, tit)
  uRel <- Rel.parseUnresolvedRelTarget (otherAttrs <> one ("title", tit)) url
  let rRel = Resolve.resolveUnresolvedRelTarget model uRel
  renderSomeInlineRefWith f id (attr, is, (url, tit)) rRel emaAction model ctx inl

plainifyWikiLinkSplice :: Monad n => PandocInlineRenderer n i b
plainifyWikiLinkSplice _emaAction _model _nf (ctxSansCustomSplicing -> ctx) _ inl = do
  s <- WL.wikiLinkInlineRendered inl
  pure $ HP.rpInline ctx $ B.Str s

inlinesWithWikiLinksPlainified :: [B.Inline] -> [B.Inline]
inlinesWithWikiLinksPlainified = W.walk $ \case
  (WL.wikiLinkInlineRendered -> Just s) ->
    -- TODO: is
    B.Str s
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

-- | Ensure that inlines list is non-empty, using the provided singleton value if necessary.
nonEmptyInlines :: Text -> [B.Inline] -> NonEmpty B.Inline
nonEmptyInlines x =
  fromMaybe (one $ B.Str x) . nonEmpty

siteRouteDefaultInnerText :: Model -> Text -> SR.SiteRoute -> Maybe [B.Inline]
siteRouteDefaultInnerText model url (SR.SiteRoute sr) =
  sr
    & absurdUnion
    `h` (\(SR.MissingR _) -> Nothing)
    `h` (\(SR.AmbiguousR _) -> Nothing)
    `h` ( \(resR :: SR.ResourceRoute) ->
            resR & absurdUnion
              `h` ( \(lmlR :: R.LMLRoute) ->
                      -- XXX: should we really lookup?
                      Tit.toInlines . MN._noteTitle <$> M.modelLookupNoteByRoute lmlR model
                  )
              `h` ( \(_ :: R.StaticFileRoute, _ :: FilePath) ->
                      -- Just append a file: prefix, to existing wiki-link.
                      pure $ B.Str "File:" : [B.Str url]
                  )
        )
    `h` (\(_ :: SR.VirtualRoute) -> Nothing)
