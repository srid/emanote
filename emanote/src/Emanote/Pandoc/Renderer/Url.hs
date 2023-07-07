module Emanote.Pandoc.Renderer.Url (
  urlResolvingSplice,
  plainifyWikiLinkSplice,
  renderSomeInlineRefWith,
) where

import Commonmark.Extensions.WikiLink qualified as WL
import Data.Text qualified as T
import Emanote.Model (Model)
import Emanote.Model qualified as M
import Emanote.Model.Link.Rel qualified as Rel
import Emanote.Model.Link.Resolve qualified as Resolve
import Emanote.Model.Note qualified as MN
import Emanote.Model.Title qualified as Tit
import Emanote.Pandoc.Link qualified as Link
import Emanote.Pandoc.Renderer (PandocInlineRenderer)
import Emanote.Route qualified as R
import Emanote.Route.SiteRoute qualified as SR
import Heist.Extra.Splices.Pandoc qualified as HP
import Heist.Extra.Splices.Pandoc qualified as Splices
import Heist.Extra.Splices.Pandoc.Ctx (ctxSansCustomSplicing)
import Heist.Interpreted qualified as HI
import Optics.Core (review)
import Relude
import Text.Pandoc.Definition qualified as B
import Text.Pandoc.Walk qualified as W

-- | Resolve all URLs in inlines (<a> and <img>)
urlResolvingSplice :: PandocInlineRenderer Model R.LMLRoute
urlResolvingSplice model _nf (ctxSansCustomSplicing -> ctx) noteRoute inl = do
  (inlRef, attr@(id', cls, otherAttrs), is, (url, tit)) <- Link.parseInlineRef inl
  let parentR = R.withLmlRoute R.routeParent noteRoute
  (uRel, mAnchor) <- Rel.parseUnresolvedRelTarget parentR (otherAttrs <> one ("title", tit)) url
  let rRel = Resolve.resolveUnresolvedRelTarget model uRel
  renderSomeInlineRefWith id (is, (url, tit)) rRel model ctx inl $ \sr ->
    case inlRef of
      Link.InlineLink -> do
        -- TODO: If uRel is `Rel.URTWikiLink (WL.WikiLinkEmbed, _)`, *and* it appears
        -- in B.Para (so do this in block-level custom splice), then embed it.
        -- We don't do this here, as this inline splice can't embed block elements.
        let (newIs, (newUrl', isNotEmaLink)) = replaceLinkNodeWithRoute model sr (is, url)
            newOtherAttrs = otherAttrs <> [openInNewTabAttr | M.inLiveServer model && isNotEmaLink]
            newAttr = (id', cls, newOtherAttrs)
            newUrl = newUrl' <> WL.anchorSuffix mAnchor
        pure $ HP.rpInline ctx $ B.Link newAttr newIs (newUrl, tit)
      Link.InlineImage -> do
        let (newIs, (newUrl, _)) =
              replaceLinkNodeWithRoute model sr (toList $ nonEmptyInlines url is, url)
        pure $ HP.rpInline ctx $ B.Image attr newIs (newUrl, tit)

openInNewTabAttr :: (Text, Text)
openInNewTabAttr =
  ("target", "_blank")

renderSomeInlineRefWith ::
  (a -> SR.SiteRoute) ->
  -- | AST Node attributes of @InlineRef@
  ([B.Inline], (Text, Text)) ->
  Rel.ResolvedRelTarget a ->
  Model ->
  Splices.RenderCtx ->
  B.Inline ->
  (a -> Maybe (HI.Splice Identity)) ->
  Maybe (HI.Splice Identity)
renderSomeInlineRefWith getSr (is, (url, tit)) rRel model (ctxSansCustomSplicing -> ctx) origInl f = do
  case rRel of
    Rel.RRTMissing -> do
      pure $ do
        raw <-
          HP.rpInline
            ctx
            ( tooltip
                "Link is broken"
                [ B.Strikeout $ one $ B.Str $ Link.unParseLink origInl
                , B.Str "❌"
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
        if M.inLiveServer model
          then pure $ raw <> details
          else pure raw
    Rel.RRTAmbiguous srs -> do
      pure $ do
        raw <- HP.rpInline ctx (tooltip "Link is ambiguous" [B.Strikeout $ one $ B.Str $ Link.unParseLink origInl, B.Str "❗"])
        candidates <-
          fmap mconcat . sequence $
            toList srs
              <&> \(getSr -> sr) -> do
                let (rp, _) = M.withoutRoutePrism model
                    srRoute = toText $ review rp sr
                    (_newIs, (newUrl, isNotEmaLink)) = replaceLinkNodeWithRoute model sr (is, srRoute)
                    linkAttr = [openInNewTabAttr | M.inLiveServer model && isNotEmaLink]
                    newIs = one $ B.Str $ show sr
                HP.rpInline ctx $
                  B.Span ("", ["emanote:error:aside"], []) $
                    one $
                      tooltip (show sr <> " -> " <> srRoute) $
                        one $
                          B.Link ("", mempty, linkAttr) newIs (newUrl, tit)
        if M.inLiveServer model
          then pure $ raw <> candidates
          else pure raw
    Rel.RRTFound sr -> do
      f sr
  where
    tooltip :: Text -> [B.Inline] -> B.Inline
    tooltip s = B.Span ("", [], one ("title", s))

plainifyWikiLinkSplice :: PandocInlineRenderer Model R.LMLRoute
plainifyWikiLinkSplice _model _nf (ctxSansCustomSplicing -> ctx) _ inl = do
  s <- WL.wikiLinkInlineRendered inl
  pure $ HP.rpInline ctx $ B.Str s

inlinesWithWikiLinksPlainified :: [B.Inline] -> [B.Inline]
inlinesWithWikiLinksPlainified = W.walk $ \case
  (WL.wikiLinkInlineRendered -> Just s) ->
    B.Str s
  x -> x

replaceLinkNodeWithRoute ::
  HasCallStack =>
  Model ->
  SR.SiteRoute ->
  ([B.Inline], Text) ->
  ([B.Inline], (Text, Bool))
replaceLinkNodeWithRoute model r (inner, url) =
  ( inlinesWithWikiLinksPlainified $ nonEmptyLinkInlines model url (Just r) inner
  , let linkUrl = SR.siteRouteUrl model r
     in (linkUrl, "?" `T.isInfixOf` linkUrl)
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

-- | Ensure that inlines list is non-empty, using the provided singleton value if necessary.
nonEmptyInlines :: Text -> [B.Inline] -> NonEmpty B.Inline
nonEmptyInlines x =
  fromMaybe (one $ B.Str x) . nonEmpty

siteRouteDefaultInnerText :: Model -> Text -> SR.SiteRoute -> Maybe [B.Inline]
siteRouteDefaultInnerText model url = \case
  SR.SiteRoute_MissingR _ -> Nothing
  SR.SiteRoute_AmbiguousR _ _ -> Nothing
  SR.SiteRoute_VirtualRoute _ -> Nothing
  SR.SiteRoute_ResourceRoute resR ->
    case resR of
      SR.ResourceRoute_LML SR.LMLView_Html lmlR ->
        Tit.toInlines . MN._noteTitle <$> M.modelLookupNoteByRoute lmlR model
      SR.ResourceRoute_LML SR.LMLView_Atom _ -> Nothing
      SR.ResourceRoute_StaticFile _ _ ->
        -- Just append a file: prefix, to existing wiki-link.
        pure $ B.Str "File:" : [B.Str url]
