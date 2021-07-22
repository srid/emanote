module Emanote.Pandoc.Renderer.BrokenLink where

import Emanote.Model (Model)
import qualified Emanote.Pandoc.Renderer.Url as Url
import qualified Heist.Extra.Splices.Pandoc as HP
import qualified Heist.Interpreted as HI
import qualified Text.Pandoc.Definition as B

-- | Like Pandoc's `Link` node, but for denoting "broken" links.
data BrokenLink
  = BrokenLink_Block B.Attr [B.Inline] (Text, Text)
  | BrokenLink_Inline B.Attr [B.Inline] (Text, Text)
  deriving (Eq, Show)

renderBrokenLink :: Monad n => Model -> HP.RenderCtx n -> Text -> BrokenLink -> HI.Splice n
renderBrokenLink model ctx err = \case
  BrokenLink_Block attr is x ->
    HP.rpBlock ctx $
      B.Div (Url.brokenLinkAttr err) $
        one . B.Para . one $
          B.Link attr (fixIs (fst x) is) x
  BrokenLink_Inline attr is x ->
    HP.rpInline ctx $
      B.Span (Url.brokenLinkAttr err) $
        one $ B.Link attr (fixIs (fst x) is) x
  where
    fixIs url is =
      Url.nonEmptyLinkInlines model url Nothing is
