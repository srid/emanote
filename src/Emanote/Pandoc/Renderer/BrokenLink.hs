module Emanote.Pandoc.Renderer.BrokenLink (BrokenLink (..), renderBrokenLink, nonEmptyInlines) where

import qualified Heist.Extra.Splices.Pandoc as HP
import Heist.Extra.Splices.Pandoc.Ctx (ctxSansCustomSplicing)
import qualified Heist.Interpreted as HI
import qualified Text.Pandoc.Definition as B

-- | Like Pandoc's `Link` node, but for denoting "broken" links.
data BrokenLink
  = BrokenLink_Block B.Attr [B.Inline] (Text, Text)
  | BrokenLink_Inline B.Attr [B.Inline] (Text, Text)
  deriving (Eq, Show)

-- | Render a broken link
--
-- Uses ctxSansCustomSplicing on the given context, so as to avoid recursing
-- into custom splices that in turns calls this function
renderBrokenLink :: Monad n => HP.RenderCtx n -> Text -> BrokenLink -> HI.Splice n
renderBrokenLink (ctxSansCustomSplicing -> ctx) err = \case
  BrokenLink_Block attr is x ->
    HP.rpBlock ctx $
      B.Div (brokenLinkAttr err) $
        one . B.Para . one $
          B.Link attr (fixIs (fst x) is) x
  BrokenLink_Inline attr is x ->
    HP.rpInline ctx $
      B.Span (brokenLinkAttr err) $
        one $ B.Link attr (fixIs (fst x) is) x
  where
    fixIs url is =
      toList $ nonEmptyInlines url is

brokenLinkAttr :: Text -> B.Attr
brokenLinkAttr err =
  ("", ["emanote:broken-link"], [("title", err)])

-- | Ensure that inlines list is non-empty, using the provided singleton value if necessary.
nonEmptyInlines :: Text -> [B.Inline] -> NonEmpty B.Inline
nonEmptyInlines x =
  fromMaybe (one $ B.Str x) . nonEmpty
