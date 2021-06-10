{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Heist.Extra.Splices.Pandoc
  ( RenderCtx (..),
    pandocSplice,
    pandocSpliceWithCustomClass,
    -- | To delegate rendering of blocks and inlines from a custom splice.
    rpBlock,
    rpInline,
    rpBlock',
    rpInline',
  )
where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Ema.Helper.Markdown as Markdown
import qualified Heist as H
import qualified Heist.Interpreted as HI
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))
import qualified Text.Pandoc.Walk as W
import qualified Text.XmlHtml as X

pandocSplice :: Monad n => Pandoc -> HI.Splice n
pandocSplice =
  pandocSpliceWithCustomClass
    mempty
    (const . const $ Nothing)
    (const . const $ Nothing)

-- | A splice to render a Pandoc AST allowing customization of the AST nodes in
-- HTML.
pandocSpliceWithCustomClass ::
  Monad n =>
  -- | How to replace classes in Div and Span nodes.
  Map Text Text ->
  -- | Custom handling of AST block nodes
  (RenderCtx n -> B.Block -> Maybe (HI.Splice n)) ->
  -- | Custom handling of AST inline nodes
  (RenderCtx n -> B.Inline -> Maybe (HI.Splice n)) ->
  Pandoc ->
  HI.Splice n
pandocSpliceWithCustomClass classMap bS iS doc = do
  node <- H.getParamNode
  let ctx =
        RenderCtx
          node
          (blockLookupAttr node)
          (inlineLookupAttr node)
          classMap
          (bS ctx)
          (iS ctx)
          (gatherFootnotes doc)
  docNodes <- renderPandocWith ctx doc
  footnotesNodes <- renderFootnotesWith (ctx {footnotes = []}) $ footnotes ctx
  pure $ docNodes <> footnotesNodes

type Footnotes = [[B.Block]]

gatherFootnotes :: Pandoc -> Footnotes
gatherFootnotes = W.query $ \case
  B.Note footnote ->
    [footnote]
  _ ->
    []

lookupFootnote :: HasCallStack => [B.Block] -> Footnotes -> Int
lookupFootnote note fs =
  fromMaybe (error $ "Missing footnote: " <> show note) $ do
    (+ 1) <$> List.elemIndex note fs

data RenderCtx n = RenderCtx
  { rootNode :: X.Node,
    -- Attributes for a given AST node.
    bAttr :: B.Block -> B.Attr,
    iAttr :: B.Inline -> B.Attr,
    -- Class attribute rewrite rules
    classMap :: Map Text Text,
    -- Custom render functions for AST nodes.
    blockSplice :: B.Block -> Maybe (HI.Splice n),
    inlineSplice :: B.Inline -> Maybe (HI.Splice n),
    -- Footnotes gathered in advance
    footnotes :: Footnotes
  }

rewriteClass :: Monad n => RenderCtx n -> B.Attr -> B.Attr
rewriteClass RenderCtx {..} (id', cls, attr) =
  let cls' = maybe cls T.words $ Map.lookup (T.intercalate " " cls) classMap
   in (id', cls', attr)

blockLookupAttr :: X.Node -> B.Block -> B.Attr
blockLookupAttr node = \case
  B.Para {} -> childTagAttr node "Para"
  B.BulletList {} -> childTagAttr node "BulletList"
  B.OrderedList {} -> childTagAttr node "OrderedList"
  B.CodeBlock {} -> childTagAttr node "CodeBlock"
  B.BlockQuote {} -> childTagAttr node "BlockQuote"
  B.Header level _ _ ->
    fromMaybe B.nullAttr $ do
      header <- X.childElementTag "Header" node
      pure $ childTagAttr header ("h" <> show level)
  _ -> B.nullAttr

inlineLookupAttr :: X.Node -> B.Inline -> B.Attr
inlineLookupAttr node = \case
  B.Code {} -> childTagAttr node "Code"
  B.Note _ ->
    childTagAttr node "Note"
  B.Link _ _ (url, _) ->
    fromMaybe B.nullAttr $ do
      link <- X.childElementTag "PandocLink" node
      let innerTag = if "://" `T.isInfixOf` url then "External" else "Internal"
      pure $ attrFromNode link `addAttr` childTagAttr link innerTag
  _ -> B.nullAttr

childTagAttr :: X.Node -> Text -> B.Attr
childTagAttr x name =
  maybe B.nullAttr attrFromNode $ X.childElementTag name x

attrFromNode :: X.Node -> B.Attr
attrFromNode node =
  let mClass = maybe mempty T.words $ X.getAttribute "class" node
      id' = fromMaybe "" $ X.getAttribute "id" node
      attrs = filter ((/= "class") . fst) $ X.elementAttrs node
   in (id', mClass, attrs)

renderFootnotesWith :: forall n. Monad n => RenderCtx n -> Footnotes -> HI.Splice n
renderFootnotesWith ctx = \case
  [] -> pure []
  footnotes -> do
    let footnotesWithIdx = zip [1 :: Int ..] footnotes
    -- TODO: Allow styling pandoc.tpl
    fmap (one . X.Element "ol" [("title", "Footnotes"), ("class", "list-decimal list-inside space-y-1 mt-4 pt-2 pl-2 border-t-2 text-gray-700")]) $
      flip foldMapM footnotesWithIdx $
        fmap (one . X.Element "li" mempty) . uncurry rpFootnote
  where
    -- TODO: Use index to create hyperlinks
    rpFootnote :: Int -> [B.Block] -> HI.Splice n
    rpFootnote _idx bs =
      one . X.Element "div" [("class", "inline-block")] <$> case bs of
        [B.Para is] ->
          -- Optimize for the most usual case, but discarding the paragraph, which adds unnecessary styling.
          foldMapM (rpInline ctx) is
        _ ->
          foldMapM (rpBlock ctx) bs

renderPandocWith :: Monad n => RenderCtx n -> Pandoc -> HI.Splice n
renderPandocWith ctx (Pandoc _meta blocks) =
  foldMapM (rpBlock ctx) blocks

rpBlock :: Monad n => RenderCtx n -> B.Block -> HI.Splice n
rpBlock ctx@RenderCtx {..} b = do
  case blockSplice b of
    Nothing ->
      rpBlock' ctx b
    Just userSplice ->
      userSplice

rpBlock' :: Monad n => RenderCtx n -> B.Block -> HI.Splice n
rpBlock' ctx@RenderCtx {..} b = case b of
  B.Plain is ->
    foldMapM (rpInline ctx) is
  B.Para is ->
    one . X.Element "p" (rpAttr $ bAttr b) <$> foldMapM (rpInline ctx) is
  B.LineBlock iss ->
    flip foldMapM iss $ \is ->
      foldMapM (rpInline ctx) is >> pure [X.TextNode "\n"]
  B.CodeBlock (id', classes, attrs) s -> do
    -- PrismJS friendly classes
    let classes' = flip concatMap classes $ \cls -> [cls, "language-" <> cls]
    pure $
      one . X.Element "div" (rpAttr $ bAttr b) $
        one . X.Element "pre" (rpAttr (id', classes', attrs)) $
          one . X.Element "code" (rpAttr ("", classes', [])) $
            one $ X.TextNode s
  B.RawBlock (B.Format fmt) s -> do
    pure $ case fmt of
      "html" ->
        rawNode "div" s
      "video" ->
        -- HACK format. TODO: replace with ![[foo.mp4]]
        one . X.Element "video" [("autoplay", ""), ("loop", ""), ("muted", "")] $
          one . X.Element "source" [("src", T.strip s)] $
            one . X.Element "p" mempty $
              [ X.TextNode "Your browser doesn't support HTML5 video. Here is a ",
                X.Element "a" [("href", T.strip s)] $
                  one . X.TextNode $ "link to the video",
                X.TextNode " instead."
              ]
      _ ->
        one . X.Element "pre" [("class", "pandoc-raw-" <> show fmt)] $ one . X.TextNode $ s
  B.BlockQuote bs ->
    one . X.Element "blockquote" (rpAttr $bAttr b) <$> foldMapM (rpBlock ctx) bs
  B.OrderedList _ bss ->
    fmap (one . X.Element "ol" (rpAttr $ bAttr b)) $
      flip foldMapM bss $
        fmap (one . X.Element "li" mempty) . foldMapM (rpBlock ctx)
  B.BulletList bss ->
    fmap (one . X.Element "ul" (rpAttr $ bAttr b)) $
      flip foldMapM bss $
        fmap (one . X.Element "li" mempty) . foldMapM (rpBlock ctx)
  B.DefinitionList defs ->
    fmap (one . X.Element "dl" mempty) $
      flip foldMapM defs $ \(term, descList) -> do
        a <- foldMapM (rpInline ctx) term
        as <-
          flip foldMapM descList $
            fmap (one . X.Element "dd" mempty) . foldMapM (rpBlock ctx)
        pure $ a <> as
  B.Header level attr is ->
    one . X.Element (headerTag level) (rpAttr $ addAttr attr $ bAttr b)
      <$> foldMapM (rpInline ctx) is
  B.HorizontalRule ->
    pure $ one $ X.Element "hr" mempty mempty
  B.Table attr _captions _colSpec (B.TableHead _ hrows) tbodys _tfoot -> do
    -- TODO: Apply captions, colSpec, etc.
    fmap (one . X.Element "table" (rpAttr attr)) $ do
      thead <- fmap (one . X.Element "thead" mempty) $
        flip foldMapM hrows $ \(B.Row _ cells) ->
          fmap (one . X.Element "tr" mempty) $
            flip foldMapM cells $ \(B.Cell _ _ _ _ blks) ->
              one . X.Element "th" mempty <$> foldMapM (rpBlock ctx) blks
      tbody <- fmap (one . X.Element "tbody" mempty) $
        flip foldMapM tbodys $ \(B.TableBody _ _ _ rows) ->
          flip foldMapM rows $ \(B.Row _ cells) ->
            fmap (one . X.Element "tr" mempty) $
              flip foldMapM cells $ \(B.Cell _ _ _ _ blks) ->
                one . X.Element "td" mempty <$> foldMapM (rpBlock ctx) blks
      pure $ thead <> tbody
  B.Div attr bs ->
    one . X.Element "div" (rpAttr $ rewriteClass ctx attr)
      <$> foldMapM (rpBlock ctx) bs
  B.Null ->
    pure []

headerTag :: HasCallStack => Int -> Text
headerTag n =
  if n >= 1 && n <= 6
    then "h" <> show n
    else error "Invalid pandoc header level"

rpInline :: Monad n => RenderCtx n -> B.Inline -> HI.Splice n
rpInline ctx@RenderCtx {..} i = do
  case inlineSplice i of
    Nothing ->
      rpInline' ctx i
    Just userSplice ->
      userSplice

rpInline' :: Monad n => RenderCtx n -> B.Inline -> HI.Splice n
rpInline' ctx@RenderCtx {..} i = case i of
  B.Str s ->
    pure $ one . X.TextNode $ s
  B.Emph is ->
    one . X.Element "em" mempty <$> foldMapM (rpInline ctx) is
  B.Strong is ->
    one . X.Element "strong" mempty <$> foldMapM (rpInline ctx) is
  B.Underline is ->
    one . X.Element "u" mempty <$> foldMapM (rpInline ctx) is
  B.Strikeout is ->
    one . X.Element "s" mempty <$> foldMapM (rpInline ctx) is
  B.Superscript is ->
    one . X.Element "sup" mempty <$> foldMapM (rpInline ctx) is
  B.Subscript is ->
    one . X.Element "sub" mempty <$> foldMapM (rpInline ctx) is
  B.Quoted qt is ->
    flip inQuotes qt $ foldMapM (rpInline ctx) is
  B.Code attr s ->
    pure $
      one . X.Element "code" (rpAttr $ addAttr attr $ iAttr i) $
        one . X.TextNode $ s
  B.Space -> pure $ one . X.TextNode $ " "
  B.SoftBreak -> pure $ one . X.TextNode $ " "
  B.LineBreak ->
    pure $ one $ X.Element "br" mempty mempty
  B.RawInline (B.Format fmt) s ->
    if fmt == "html"
      then pure $ rawNode "span" s
      else
        pure $
          one . X.Element "pre" [("class", "pandoc-raw-" <> show fmt)] $
            one . X.TextNode $ s
  B.Math mathType s ->
    case mathType of
      B.InlineMath ->
        pure $
          one . X.Element "span" [("class", "math inline")] $
            one . X.TextNode $ "\\(" <> s <> "\\)"
      B.DisplayMath ->
        pure $
          one . X.Element "span" [("class", "math display")] $
            one . X.TextNode $ "$$" <> s <> "$$"
  B.Link attr is (url, tit) -> do
    let attrs = [("href", url), ("title", tit)] <> rpAttr (addAttr attr $ iAttr i)
    one . X.Element "a" attrs <$> foldMapM (rpInline ctx) is
  B.Image attr is (url, tit) -> do
    let attrs = [("src", url), ("title", tit), ("alt", Markdown.plainify is)] <> rpAttr attr
    pure $ one . X.Element "img" attrs $ mempty
  B.Note bs -> do
    let idx = lookupFootnote bs footnotes
    -- TODO: Allow styling via pandoc.tpl
    pure $
      one $
        X.Element "sup" [("class", "px-0.5")] $ one . X.TextNode $ show idx
  B.Span attr is ->
    one . X.Element "span" (rpAttr $ rewriteClass ctx attr) <$> foldMapM (rpInline ctx) is
  x ->
    -- TODO: Implement these
    pure $ one . X.Element "pre" mempty $ one . X.TextNode $ show x
  where
    inQuotes :: Monad n => HI.Splice n -> B.QuoteType -> HI.Splice n
    inQuotes w = \case
      B.SingleQuote ->
        w <&> \nodes ->
          [X.TextNode "‘"] <> nodes <> [X.TextNode "’"]
      B.DoubleQuote ->
        w <&> \nodes ->
          [X.TextNode "“"] <> nodes <> [X.TextNode "”"]

-- | Convert Pandoc attributes to XmlHtml attributes
rpAttr :: B.Attr -> [(Text, Text)]
rpAttr (id', classes, attrs) =
  let cls = T.intercalate " " classes
   in unlessNull id' [("id", id')]
        <> unlessNull cls [("class", cls)]
        <> mconcat (fmap (\(k, v) -> [(k, v)]) attrs)
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

rawNode :: Text -> Text -> [X.Node]
rawNode wrapperTag s =
  one . X.Element wrapperTag (one ("xmlhtmlRaw", "")) $
    one . X.TextNode $ s
