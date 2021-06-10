{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Heist.Extra.Splices.Pandoc.Render
  ( renderPandocWith,
    rpBlock,
    rpInline,
    rpBlock',
    rpInline',
    plainify,
  )
where

import qualified Data.Text as T
import Heist.Extra.Splices.Pandoc.Attr (addAttr, rpAttr)
import Heist.Extra.Splices.Pandoc.Ctx
  ( RenderCtx (..),
    rewriteClass,
  )
import qualified Heist.Interpreted as HI
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))
import qualified Text.Pandoc.Walk as W
import qualified Text.XmlHtml as X

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
    let attrs = [("src", url), ("title", tit), ("alt", plainify is)] <> rpAttr attr
    pure $ one . X.Element "img" attrs $ mempty
  B.Note bs -> do
    one . X.Element "aside" mempty
      <$> foldMapM (rpBlock ctx) bs
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

rawNode :: Text -> Text -> [X.Node]
rawNode wrapperTag s =
  one . X.Element wrapperTag (one ("xmlhtmlRaw", "")) $
    one . X.TextNode $ s

-- | Convert Pandoc AST inlines to raw text.
plainify :: [B.Inline] -> Text
plainify = W.query $ \case
  B.Str x -> x
  B.Code _attr x -> x
  B.Space -> " "
  B.SoftBreak -> " "
  B.LineBreak -> " "
  B.RawInline _fmt s -> s
  -- TODO: How to wrap math stuff here?
  B.Math _mathTyp s -> s
  -- Ignore the rest of AST nodes, as they are recursively defined in terms of
  -- `Inline` which `W.query` will traverse again.
  _ -> ""