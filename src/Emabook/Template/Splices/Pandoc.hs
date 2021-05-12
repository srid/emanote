{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

module Emabook.Template.Splices.Pandoc (renderPandoc) where

import Control.Exception (throw)
import qualified Data.Text as T
import qualified Ema.Helper.Markdown as Markdown
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))

-- TODO: Use heist splice to override CSS classes

renderPandoc :: Pandoc -> H.Html
renderPandoc (Pandoc _meta blocks) =
  mapM_ rpBlock blocks

rpBlock :: B.Block -> H.Html
rpBlock = \case
  B.Plain is ->
    mapM_ rpInline is
  B.Para is ->
    H.p ! A.class_ "my-2" $ mapM_ rpInline is
  B.LineBlock iss ->
    forM_ iss $ \is ->
      mapM_ rpInline is >> "\n"
  B.CodeBlock (id', classes, attrs) s ->
    -- Prism friendly classes
    let classes' = flip concatMap classes $ \cls -> [cls, "language-" <> cls]
     in H.div ! A.class_ "py-0.5 text-sm" $ H.pre ! rpAttr (id', classes', attrs) $ H.code ! rpAttr ("", classes', []) $ H.text s
  B.RawBlock (B.Format fmt) rawHtml ->
    if fmt == "html"
      then H.unsafeByteString $ encodeUtf8 rawHtml
      else throw Unsupported
  B.BlockQuote bs ->
    H.blockquote $ mapM_ rpBlock bs
  B.OrderedList _ bss ->
    H.ol ! A.class_ (listStyle <> " list-decimal") $
      forM_ bss $ \bs ->
        H.li ! A.class_ listItemStyle $ mapM_ rpBlock bs
  B.BulletList bss ->
    H.ul ! A.class_ (listStyle <> " list-disc") $
      forM_ bss $ \bs ->
        H.li ! A.class_ listItemStyle $ mapM_ rpBlock bs
  B.DefinitionList defs ->
    H.dl $
      forM_ defs $ \(term, descList) -> do
        mapM_ rpInline term
        forM_ descList $ \desc ->
          H.dd $ mapM_ rpBlock desc
  B.Header level attr is ->
    headerElem level ! rpAttr attr $ mapM_ rpInline is
  B.HorizontalRule ->
    H.hr
  B.Table {} ->
    throw Unsupported
  B.Div attr bs ->
    H.div ! rpAttr attr $ mapM_ rpBlock bs
  B.Null ->
    pure ()
  where
    listStyle = "list-inside ml-4 space-y-1 "
    listItemStyle = ""

headerElem :: Int -> H.Html -> H.Html
headerElem = \case
  1 -> H.h1 ! A.class_ "text-6xl mt-2 mb-2 text-center pb-2"
  2 -> H.h2 ! A.class_ ("text-5xl " <> my)
  3 -> H.h3 ! A.class_ ("text-4xl " <> my)
  4 -> H.h4 ! A.class_ ("text-3xl " <> my)
  5 -> H.h5 ! A.class_ ("text-2xl " <> my)
  6 -> H.h6 ! A.class_ ("text-xl " <> my)
  _ -> error "Invalid pandoc header level"
  where
    my = "mt-4 mb-2 text-gray-700"

rpInline :: B.Inline -> H.Html
rpInline = \case
  B.Str s -> H.toHtml s
  B.Emph is ->
    H.em $ mapM_ rpInline is
  B.Strong is ->
    H.strong $ mapM_ rpInline is
  B.Underline is ->
    H.u $ mapM_ rpInline is
  B.Strikeout is ->
    -- FIXME: Should use <s>, but blaze doesn't have it.
    H.del $ mapM_ rpInline is
  B.Superscript is ->
    H.sup $ mapM_ rpInline is
  B.Subscript is ->
    H.sub $ mapM_ rpInline is
  B.Quoted qt is ->
    flip inQuotes qt $ mapM_ rpInline is
  B.Code attr s ->
    H.code ! rpAttr attr $ H.toHtml s
  B.Space -> " "
  B.SoftBreak -> " "
  B.LineBreak -> H.br
  B.RawInline _fmt s ->
    H.pre $ H.toHtml s
  B.Math _ _ ->
    throw Unsupported
  B.Link attr is (url, tit) -> do
    let (cls, target) =
          if "://" `T.isInfixOf` url
            then ("text-pink-600 hover:underline", targetBlank)
            else ("text-pink-600 font-bold hover:bg-pink-50", mempty)
    H.a
      ! A.class_ cls
      ! A.href (H.textValue url)
      ! A.title (H.textValue tit)
      ! target
      ! rpAttr attr
      $ mapM_ rpInline is
  B.Image attr is (url, tit) ->
    H.img ! A.src (H.textValue url) ! A.title (H.textValue tit) ! A.alt (H.textValue $ Markdown.plainify is) ! rpAttr attr
  B.Note _ ->
    throw Unsupported
  B.Span attr is ->
    H.span ! rpAttr attr $ mapM_ rpInline is
  x ->
    H.pre $ H.toHtml $ show @Text x
  where
    inQuotes :: H.Html -> B.QuoteType -> H.Html
    inQuotes w = \case
      B.SingleQuote -> "‘" >> w <* "’"
      B.DoubleQuote -> "“" >> w <* "”"

targetBlank :: H.Attribute
targetBlank =
  A.target "_blank" <> A.rel "noopener"

rpAttr :: B.Attr -> H.Attribute
rpAttr (id', classes, attrs) =
  let cls = T.intercalate " " classes
   in unlessNull id' (A.id (fromString . toString $ id'))
        <> unlessNull cls (A.class_ (fromString . toString $ cls))
        <> mconcat (fmap (\(k, v) -> H.dataAttribute (fromString . toString $ k) (fromString . toString $ v)) attrs)
  where
    unlessNull x f =
      if T.null x then mempty else f

data Unsupported = Unsupported
  deriving (Show, Exception)
