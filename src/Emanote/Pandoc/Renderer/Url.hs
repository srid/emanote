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
import Emanote.Model (Model)
import qualified Emanote.Model as M
import qualified Emanote.Model.Link.Rel as Rel
import qualified Emanote.Model.Link.Resolve as Resolve
import qualified Emanote.Model.Note as MN
import qualified Emanote.Model.Title as Tit
import qualified Emanote.Pandoc.Link as Link
import qualified Emanote.Pandoc.Markdown.Syntax.WikiLink as WL
import Emanote.Pandoc.Renderer (PandocInlineRenderer)
import Emanote.Prelude (h)
import qualified Emanote.Route as R
import qualified Emanote.Route.SiteRoute as SR
import qualified Heist.Extra.Splices.Pandoc as HP
import qualified Heist.Extra.Splices.Pandoc as Splices
import Heist.Extra.Splices.Pandoc.Ctx (ctxSansCustomSplicing)
import qualified Heist.Interpreted as HI
import Relude
import qualified Text.Pandoc.Definition as B
import qualified Text.Pandoc.Walk as W

-- | Resolve all URLs in inlines (<a> and <img>)
urlResolvingSplice :: Monad n => PandocInlineRenderer n i b
urlResolvingSplice emaAction model _nf (ctxSansCustomSplicing -> ctx) _ inl = do
  (inlRef, attr@(id', cls, otherAttrs), is, (url, tit)) <- Link.parseInlineRef inl
  let f = \sr -> do
        case inlRef of
          Link.InlineLink -> do
            -- TODO: If uRel is `Rel.URTWikiLink (WL.WikiLinkEmbed, _)`, *and* it appears
            -- in B.Para (so do this in block-level custom splice), then embed it.
            -- We don't do this here, as this inline splice can't embed block elements.
            let (newIs, (newUrl, isFileLink)) = replaceLinkNodeWithRoute emaAction model sr (is, url)
                newAttr = (id', cls, otherAttrs <> bool mempty (fileLinkAttr emaAction) isFileLink)
            pure $ HP.rpInline ctx $ B.Link newAttr newIs (newUrl, tit)
          Link.InlineImage -> do
            let (newIs, (newUrl, _)) =
                  replaceLinkNodeWithRoute emaAction model sr (toList $ nonEmptyInlines url is, url)
            pure $ HP.rpInline ctx $ B.Image attr newIs (newUrl, tit)
  uRel <- Rel.parseUnresolvedRelTarget (otherAttrs <> one ("title", tit)) url
  let rRel = Resolve.resolveUnresolvedRelTarget model uRel
  renderSomeInlineRefWith f id (is, (url, tit)) rRel emaAction model ctx inl

fileLinkAttr :: Ema.CLI.Action -> [(Text, Text)]
fileLinkAttr emaAction =
  [("target", "_blank") | emaAction == Ema.CLI.Run]

renderSomeInlineRefWith ::
  Monad n =>
  (a -> Maybe (HI.Splice n)) ->
  (a -> (SR.SiteRoute, Maybe UTCTime)) ->
  -- | AST Node attributes of @InlineRef@
  ([B.Inline], (Text, Text)) ->
  Rel.ResolvedRelTarget a ->
  Ema.CLI.Action ->
  Model ->
  Splices.RenderCtx n ->
  B.Inline ->
  Maybe (HI.Splice n)
renderSomeInlineRefWith f getSr (is, (url, tit)) rRel emaAction model (ctxSansCustomSplicing -> ctx) origInl = do
  case rRel of
    Rel.RRTMissing -> do
      pure $ do
        raw <-
          HP.rpInline
            ctx
            ( tooltip
                "Link is broken"
                [ B.Strikeout $ one $ B.Str $ Link.unParseLink origInl,
                  B.Str "❌"
                ]
            )
        details <-
          HP.rpInline ctx $
            -- FIXME: This aside is meaningless for non-wikilink links (regular
            -- Markdown links)
            B.Span ("", ["emanote:error:aside"], []) $
              one $
                tooltip "Find notes containing this broken link" $
                  one $
                    B.Link B.nullAttr (one $ B.Emph $ one $ B.Str "backlinks") (url, "")
        if emaAction == Ema.CLI.Run
          then pure $ raw <> details
          else pure raw
    Rel.RRTAmbiguous srs -> do
      pure $ do
        raw <- HP.rpInline ctx (tooltip "Link is ambiguous" [B.Strikeout $ one $ B.Str $ Link.unParseLink origInl, B.Str "❗"])
        candidates <-
          fmap mconcat . sequence $
            toList srs
              <&> \(getSr -> sr) -> do
                let srRoute = toText $ Ema.encodeRoute model (fst sr)
                    (_newIs, (newUrl, isFileLink)) = replaceLinkNodeWithRoute emaAction model sr (is, srRoute)
                    linkAttr = bool mempty (fileLinkAttr emaAction) isFileLink
                    newIs = one $ B.Str $ show $ fst sr
                HP.rpInline ctx $
                  B.Span ("", ["emanote:error:aside"], []) $
                    one $
                      tooltip (show (fst sr) <> " -> " <> srRoute) $
                        one $
                          B.Link ("", mempty, linkAttr) newIs (newUrl, tit)
        if emaAction == Ema.CLI.Run
          then pure $ raw <> candidates
          else pure raw
    Rel.RRTFound sr -> do
      f sr
  where
    tooltip :: Text -> [B.Inline] -> B.Inline
    tooltip s = B.Span ("", [], one ("title", s))

plainifyWikiLinkSplice :: Monad n => PandocInlineRenderer n i b
plainifyWikiLinkSplice _emaAction _model _nf (ctxSansCustomSplicing -> ctx) _ inl = do
  s <- WL.wikiLinkInlineRendered inl
  pure $ HP.rpInline ctx $ B.Str s

inlinesWithWikiLinksPlainified :: [B.Inline] -> [B.Inline]
inlinesWithWikiLinksPlainified = W.walk $ \case
  (WL.wikiLinkInlineRendered -> Just s) ->
    B.Str s
  x -> x

replaceLinkNodeWithRoute ::
  HasCallStack =>
  Ema.CLI.Action ->
  Model ->
  (SR.SiteRoute, Maybe UTCTime) ->
  ([B.Inline], Text) ->
  ([B.Inline], (Text, Bool))
replaceLinkNodeWithRoute emaAction model (r, mTime) (inner, url) =
  ( inlinesWithWikiLinksPlainified $ nonEmptyLinkInlines model url (Just r) inner,
    siteRouteUrlWithTime emaAction model (r, mTime)
  )
  where
    nonEmptyLinkInlines :: Model -> Text -> Maybe SR.SiteRoute -> [B.Inline] -> [B.Inline]
    nonEmptyLinkInlines model' url' mr = \case
      [] ->
        toList $
          nonEmptyInlines url $
            fromMaybe [] $
              siteRouteDefaultInnerText model' url' =<< mr
      x -> x

siteRouteUrlWithTime :: Ema.CLI.Action -> Model -> (SR.SiteRoute, Maybe UTCTime) -> (Text, Bool)
siteRouteUrlWithTime emaAction model (SR.siteRouteUrl model -> linkUrl, mUrlTime) =
  -- In live server mode, append last modification time if any, such
  -- that the browser is forced to refresh the inline image on hot
  -- reload (Ema's DOM patch).
  fromMaybe
    (linkUrl, False)
    ( do
        guard $ emaAction == Ema.CLI.Run
        t <- mUrlTime
        let u = linkUrl <> toText ("?t=" <> formatTime defaultTimeLocale "%s" t)
        pure (u, True)
    )

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
                      Tit.toInlines . MN._noteTitle <$> M.modelLookupNoteByRoute lmlR model
                  )
              `h` ( \(_ :: R.StaticFileRoute, _ :: FilePath) ->
                      -- Just append a file: prefix, to existing wiki-link.
                      pure $ B.Str "File:" : [B.Str url]
                  )
        )
    `h` (\(_ :: SR.VirtualRoute) -> Nothing)
