module Emanote.Pandoc.Renderer.Tag (
  tagLinkSplice,
) where

import Emanote.Model (Model)
import Emanote.Pandoc.Markdown.Syntax.HashTag qualified as HT
import Emanote.Pandoc.Renderer (PandocInlineRenderer)
import Emanote.Route qualified as R
import Emanote.Route.SiteRoute.Class qualified as SR
import Heist.Extra.Splices.Pandoc qualified as HP
import Heist.Extra.Splices.Pandoc.Ctx (ctxSansCustomSplicing)
import Relude
import Text.Pandoc.Definition qualified as B

{- | Render semantic hashtag spans as links to their tag index page.

This lives in the renderer rather than 'preparePandoc' so tag URLs use the
model-aware site route encoder.
-}
tagLinkSplice :: PandocInlineRenderer Model R.LMLRoute
tagLinkSplice model _nr (ctxSansCustomSplicing -> ctx) _route inline = do
  tag <- HT.getTagFromInline inline
  B.Span attr inlines <- pure inline
  let tagUrl = SR.siteRouteUrl model $ SR.tagIndexRoute $ toList $ HT.deconstructTag tag
  pure $ HP.rpInline ctx $ B.Span attr [B.Link mempty inlines (tagUrl, "Tag")]
