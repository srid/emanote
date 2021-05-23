{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Heist.Extra.Splices.Pandoc (pandocSplice, pandocSpliceWithCustomClass) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Ema.Helper.Markdown as Markdown
import qualified Heist as H
import qualified Heist.Interpreted as HI
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Renderer.XmlHtml as RX
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))
import qualified Text.XmlHtml as XmlHtml

pandocSplice :: Monad n => Pandoc -> HI.Splice n
pandocSplice = pandocSpliceWithCustomClass mempty

-- | A splice to render a Pandoc AST allowing customization of the AST nodes in
-- HTML.
pandocSpliceWithCustomClass ::
  Monad n =>
  -- | How to replace classes in Div and Span nodes.
  Map Text Text ->
  Pandoc ->
  HI.Splice n
pandocSpliceWithCustomClass classMap doc = do
  node <- H.getParamNode
  let ctx = RenderCtx (blockLookupAttr node) (inlineLookupAttr node) classMap
  pure $ RX.renderHtmlNodes $ renderPandocWith ctx doc

data RenderCtx = RenderCtx
  { bAttr :: B.Block -> B.Attr,
    iAttr :: B.Inline -> B.Attr,
    classMap :: Map Text Text
  }

rewriteClass :: RenderCtx -> B.Attr -> B.Attr
rewriteClass RenderCtx {..} (id', cls, attr) =
  let cls' = maybe cls T.words $ Map.lookup (T.intercalate " " cls) classMap
   in (id', cls', attr)

blockLookupAttr :: XmlHtml.Node -> B.Block -> B.Attr
blockLookupAttr node = \case
  B.Para {} -> childTagAttr node "Para"
  B.BulletList {} -> childTagAttr node "BulletList"
  B.OrderedList {} -> childTagAttr node "OrderedList"
  B.CodeBlock {} -> childTagAttr node "CodeBlock"
  B.BlockQuote {} -> childTagAttr node "BlockQuote"
  B.Header level _ _ ->
    fromMaybe B.nullAttr $ do
      header <- XmlHtml.childElementTag "Header" node
      pure $ childTagAttr header ("h" <> show level)
  _ -> B.nullAttr

inlineLookupAttr :: XmlHtml.Node -> B.Inline -> B.Attr
inlineLookupAttr node = \case
  B.Code {} -> childTagAttr node "Code"
  B.Note _ ->
    childTagAttr node "Note"
  B.Link _ _ (url, _) ->
    fromMaybe B.nullAttr $ do
      link <- XmlHtml.childElementTag "PandocLink" node
      let innerTag = if "://" `T.isInfixOf` url then "External" else "Internal"
      pure $ attrFromNode link `addAttr` childTagAttr link innerTag
  _ -> B.nullAttr

childTagAttr :: XmlHtml.Node -> Text -> B.Attr
childTagAttr x name =
  maybe B.nullAttr attrFromNode $ XmlHtml.childElementTag name x

attrFromNode :: XmlHtml.Node -> B.Attr
attrFromNode node =
  let mClass = maybe mempty T.words $ XmlHtml.getAttribute "class" node
      id' = fromMaybe "" $ XmlHtml.getAttribute "id" node
      attrs = filter ((/= "class") . fst) $ XmlHtml.elementAttrs node
   in (id', mClass, attrs)

renderPandocWith :: RenderCtx -> Pandoc -> H.Html
renderPandocWith ctx (Pandoc _meta blocks) =
  mapM_ (rpBlock ctx) blocks

rpBlock :: RenderCtx -> B.Block -> H.Html
rpBlock ctx@RenderCtx {..} b = case b of
  B.Plain is ->
    mapM_ (rpInline ctx) is
  B.Para is ->
    H.p ! rpAttr (bAttr b) $ mapM_ (rpInline ctx) is
  B.LineBlock iss ->
    forM_ iss $ \is ->
      mapM_ (rpInline ctx) is >> "\n"
  B.CodeBlock (id', classes, attrs) s ->
    -- PrismJS friendly classes
    let classes' = flip concatMap classes $ \cls -> [cls, "language-" <> cls]
     in H.div ! rpAttr (bAttr b) $
          H.pre ! rpAttr (id', classes', attrs) $
            H.code ! rpAttr ("", classes', []) $ H.text s
  B.RawBlock (B.Format fmt) s -> do
    case fmt of
      "html" ->
        H.unsafeByteString $ encodeUtf8 s
      "video" ->
        -- HACK format. TODO: replace with ![[foo.mp4]]
        H.video ! A.autoplay "" ! A.loop "" ! H.customAttribute "muted" "" $ do
          H.source ! A.src (H.toValue $ T.strip s)
          H.p $ do
            "Your browser doesn't support HTML5 video. Here is a "
            H.a ! A.href (H.toValue $ T.strip s) $ "link to the video"
            " instead."
      _ ->
        H.pre ! A.class_ ("pandoc-raw-" <> show fmt) $ H.text s
  B.BlockQuote bs ->
    H.blockquote ! rpAttr (bAttr b) $ mapM_ (rpBlock ctx) bs
  B.OrderedList _ bss ->
    H.ol ! rpAttr (bAttr b) $
      forM_ bss $ \bs ->
        H.li $ mapM_ (rpBlock ctx) bs
  B.BulletList bss ->
    H.ul ! rpAttr (bAttr b) $
      forM_ bss $ \bs ->
        H.li $ mapM_ (rpBlock ctx) bs
  B.DefinitionList defs ->
    H.dl $
      forM_ defs $ \(term, descList) -> do
        mapM_ (rpInline ctx) term
        forM_ descList $ \desc ->
          H.dd $ mapM_ (rpBlock ctx) desc
  B.Header level attr is ->
    headerElem level ! rpAttr (addAttr attr $ bAttr b) $ mapM_ (rpInline ctx) is
  B.HorizontalRule ->
    H.hr
  B.Table attr _captions _colSpec (B.TableHead _ hrows) tbodys _tfoot ->
    -- TODO: Apply captions, colSpec, etc.
    H.table ! rpAttr attr $ do
      H.thead $ do
        forM_ hrows $ \(B.Row _ cells) ->
          H.tr $
            forM_ cells $ \(B.Cell _ _ _ _ blks) ->
              H.th $ rpBlock ctx `mapM_` blks
      H.tbody $ do
        forM_ tbodys $ \(B.TableBody _ _ _ rows) ->
          forM_ rows $ \(B.Row _ cells) ->
            H.tr $
              forM_ cells $ \(B.Cell _ _ _ _ blks) ->
                H.td $ rpBlock ctx `mapM_` blks
  B.Div attr bs ->
    H.div ! rpAttr (rewriteClass ctx attr) $ mapM_ (rpBlock ctx) bs
  B.Null ->
    pure ()

headerElem :: Int -> H.Html -> H.Html
headerElem = \case
  1 -> H.h1
  2 -> H.h2
  3 -> H.h3
  4 -> H.h4
  5 -> H.h5
  6 -> H.h6
  _ -> error "Invalid pandoc header level"

rpInline :: RenderCtx -> B.Inline -> H.Html
rpInline ctx@RenderCtx {..} i = case i of
  B.Str s -> H.toHtml s
  B.Emph is ->
    H.em $ mapM_ (rpInline ctx) is
  B.Strong is ->
    H.strong $ mapM_ (rpInline ctx) is
  B.Underline is ->
    H.u $ mapM_ (rpInline ctx) is
  B.Strikeout is ->
    H.del $ mapM_ (rpInline ctx) is
  B.Superscript is ->
    H.sup $ mapM_ (rpInline ctx) is
  B.Subscript is ->
    H.sub $ mapM_ (rpInline ctx) is
  B.Quoted qt is ->
    flip inQuotes qt $ mapM_ (rpInline ctx) is
  B.Code attr s ->
    H.code ! rpAttr (addAttr attr $ iAttr i) $ H.toHtml s
  B.Space -> " "
  B.SoftBreak -> " "
  B.LineBreak -> H.br
  B.RawInline (B.Format fmt) s ->
    if fmt == "html"
      then H.unsafeByteString $ encodeUtf8 s
      else H.pre ! A.class_ ("pandoc-raw-" <> show fmt) $ H.toHtml s
  B.Math mathType s ->
    case mathType of
      B.InlineMath ->
        H.span ! A.class_ "math inline" $ H.text $ "\\(" <> s <> "\\)"
      B.DisplayMath ->
        H.span ! A.class_ "math display" $ do
          "$$"
          H.text s
          "$$"
  B.Link attr is (url, tit) -> do
    H.a
      ! A.href (H.textValue url)
      ! A.title (H.textValue tit)
      ! rpAttr (addAttr attr $ iAttr i)
      $ mapM_ (rpInline ctx) is
  B.Image attr is (url, tit) ->
    H.img ! A.src (H.textValue url) ! A.title (H.textValue tit) ! A.alt (H.textValue $ Markdown.plainify is) ! rpAttr attr
  B.Note bs -> do
    -- TODO: Style this properly to be Tufte like. Maybe integrate https://edwardtufte.github.io/tufte-css/
    H.sup ! A.style "margin-left: 3px;" $ "note"
    H.div ! rpAttr (iAttr i) $ mapM_ (rpBlock ctx) bs
  B.Span attr is ->
    H.span ! rpAttr (rewriteClass ctx attr) $ mapM_ (rpInline ctx) is
  x ->
    H.pre $ H.toHtml $ show @Text x
  where
    inQuotes :: H.Html -> B.QuoteType -> H.Html
    inQuotes w = \case
      B.SingleQuote -> "‘" >> w <* "’"
      B.DoubleQuote -> "“" >> w <* "”"

rpAttr :: B.Attr -> H.Attribute
rpAttr (id', classes, attrs) =
  let cls = T.intercalate " " classes
   in unlessNull id' (A.id (fromString . toString $ id'))
        <> unlessNull cls (A.class_ (fromString . toString $ cls))
        <> mconcat (fmap (\(k, v) -> H.customAttribute (fromString . toString $ k) (fromString . toString $ v)) attrs)
  where
    unlessNull x f =
      if T.null x then mempty else f

addAttr :: B.Attr -> B.Attr -> B.Attr
addAttr (id1, cls1, attr1) (id2, cls2, attr2) =
  (pickNonNull id1 id2, cls1 <> cls2, attr1 <> attr2)
  where
    pickNonNull x "" = x
    pickNonNull "" x = x
    pickNonNull _ _ = ""
