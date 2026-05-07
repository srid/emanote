module Emanote.Pandoc.Renderer.Url (
  urlResolvingSplice,
  plainifyWikiLinkSplice,
  renderSomeInlineRefWith,
) where

import Commonmark.Extensions.WikiLink qualified as WL
import Data.Map.Syntax ((##))
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
import Heist.Extra qualified as HE
import Heist.Extra.Splices.List (listSplice)
import Heist.Extra.Splices.Pandoc qualified as HP
import Heist.Extra.Splices.Pandoc qualified as Splices
import Heist.Extra.Splices.Pandoc.Ctx (ctxSansCustomSplicing)
import Heist.Interpreted qualified as HI
import Heist.Splices qualified as Heist
import Optics.Core (review)
import Relude
import Text.Pandoc.Definition qualified as B
import Text.Pandoc.Walk qualified as W

-- | Resolve all URLs in inlines (<a> and <img>)
urlResolvingSplice :: PandocInlineRenderer Model R.LMLRoute
urlResolvingSplice model _nf (ctxSansCustomSplicing -> ctx) noteRoute inl = do
  (inlRef, attr@(id', cls, otherAttrs), is, (url, tit)) <- Link.parseInlineRef inl
  let parentR = M.modelResolveLinkBase model noteRoute
  (uRel, mAnchor) <- Rel.parseUnresolvedRelTarget parentR (otherAttrs <> one ("title", tit)) url
  let rRel = Resolve.resolveUnresolvedRelTarget model noteRoute uRel
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
  -- | AST Node attributes of @InlineRef@ — kept in the signature for symmetry
  -- with callers; the missing/ambiguous arms now route through templates and
  -- don't need it. The 'RRTFound' arm passes 'sr' to @f@.
  ([B.Inline], (Text, Text)) ->
  Rel.ResolvedRelTarget a ->
  Model ->
  Splices.RenderCtx ->
  B.Inline ->
  (a -> Maybe (HI.Splice Identity)) ->
  Maybe (HI.Splice Identity)
renderSomeInlineRefWith getSr _ rRel model (ctxSansCustomSplicing -> _ctx) origInl f =
  case rRel of
    Rel.RRTMissing -> do
      let linkText = Link.unParseLink origInl
      pure $ do
        tpl <- HE.lookupHtmlTemplateMust "/templates/components/broken-link"
        HE.runCustomTemplate tpl
          $ "ema:broken-link:text"
          ## HI.textSplice linkText
    Rel.RRTAmbiguous srs -> do
      let linkText = Link.unParseLink origInl
          mkCandidate (getSr -> sr) =
            let (rp, _) = M.withoutRoutePrism model
                srRoute = toText $ review rp sr
                candUrl = SR.siteRouteUrl model sr
                isNotEmaLink = "?" `T.isInfixOf` candUrl
                candLabel = show sr
                candTooltip = candLabel <> " -> " <> srRoute
             in (candLabel, candUrl, isNotEmaLink, candTooltip)
          -- Candidates only render in live preview — in static export the URL
          -- has already resolved to one of them via closest-ancestor disambiguation.
          candidates =
            if M.inLiveServer model
              then mkCandidate <$> toList srs
              else []
          candidatesSplice =
            listSplice candidates "each-candidate" $ \(label, candUrl, isNotEmaLink, candTooltip) -> do
              "ema:candidate:label" ## HI.textSplice label
              "ema:candidate:url" ## HI.textSplice candUrl
              "ema:candidate:newtab" ## Heist.ifElseISplice isNotEmaLink
              "ema:candidate:tooltip" ## HI.textSplice candTooltip
      pure $ do
        tpl <- HE.lookupHtmlTemplateMust "/templates/components/ambiguous-link"
        HE.runCustomTemplate tpl $ do
          "ema:ambiguous-link:text" ## HI.textSplice linkText
          "ema:ambiguous-link:candidates" ## candidatesSplice
    Rel.RRTFound sr -> do
      f sr

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
  (HasCallStack) =>
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
        toList
          $ nonEmptyInlines url
          $ fromMaybe []
          $ siteRouteDefaultInnerText model' url'
          =<< mr
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
      SR.ResourceRoute_LML R.LMLView_Html lmlR ->
        Tit.toInlines . MN._noteTitle <$> M.modelLookupNoteByRoute' lmlR model
      SR.ResourceRoute_LML R.LMLView_Atom _ -> Nothing
      SR.ResourceRoute_StaticFile _ _ ->
        -- Just append a file: prefix, to existing wiki-link.
        pure $ B.Str "File:" : [B.Str url]
