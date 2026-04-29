{-# LANGUAGE RecordWildCards #-}

{- |  Obsidian-style callouts

  TODO: Should we switch to using the commonmark-hs parser here? cf. https://github.com/jgm/commonmark-hs/pull/135
-}
module Emanote.Pandoc.Renderer.Callout (
  calloutResolvingSplice,

  -- * For tests
  CalloutType (..),
  Callout (..),
  FoldState (..),
  parseCallout,
  parseCalloutType,
  parseCalloutHeader,
) where

import Data.Default (Default (def))
import Data.Map.Syntax ((##))
import Data.Text qualified as T
import Emanote.Model (Model)
import Emanote.Model.Title qualified as Tit
import Emanote.Pandoc.Renderer (PandocBlockRenderer)
import Emanote.Route (LMLRoute)
import Heist.Extra qualified as HE
import Heist.Extra.Splices.Pandoc qualified as HP
import Heist.Interpreted qualified as HI
import Heist.Splices qualified as HS
import Relude
import Text.Casing qualified
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.Pandoc.Definition qualified as B

calloutResolvingSplice :: PandocBlockRenderer Model LMLRoute
calloutResolvingSplice _model _nr _embedStack ctx _noteRoute blk = do
  B.BlockQuote blks <- pure blk
  callout <- parseCallout blks
  let calloutType = T.toLower $ unCalloutType $ type_ callout
  pure $ do
    tpl <- HE.lookupHtmlTemplateMust $ "/templates/filters/callout/" <> encodeUtf8 calloutType
    HE.runCustomTemplate tpl $ do
      "callout:type" ## HI.textSplice calloutType
      "callout:title" ## Tit.titleSplice ctx id $ Tit.fromInlines (title callout)
      "callout:body" ## HP.pandocSplice ctx $ B.Pandoc mempty (body callout)
      "callout:fold-state" ## HI.textSplice (foldStateAttr $ foldState callout)
      "callout:if-not-foldable" ## HS.ifISplice (isNothing $ foldState callout)
      "callout:if-foldable-expanded" ## HS.ifISplice (foldState callout == Just Expanded)
      "callout:if-foldable-collapsed" ## HS.ifISplice (foldState callout == Just Collapsed)
      "query" ##
        HI.textSplice (show blks)
  where
    foldStateAttr :: Maybe FoldState -> Text
    foldStateAttr = \case
      Nothing -> ""
      Just Expanded -> "expanded"
      Just Collapsed -> "collapsed"

{- | Obsidian callout type

TODO: Add the rest, from https://help.obsidian.md/Editing+and+formatting/Callouts#Supported%20types
-}
newtype CalloutType = CalloutType {unCalloutType :: Text}
  deriving stock (Eq, Ord, Show)

instance Default CalloutType where
  def = CalloutType "note"

-- | Initial fold state of a foldable callout.
data FoldState
  = -- | @[!type]+@: foldable, initially expanded
    Expanded
  | -- | @[!type]-@: foldable, initially collapsed
    Collapsed
  deriving stock (Eq, Ord, Show)

data Callout = Callout
  { type_ :: CalloutType
  , title :: [B.Inline]
  , body :: [B.Block]
  , foldState :: Maybe FoldState
  -- ^ 'Nothing' if the callout is not foldable.
  }
  deriving stock (Eq, Ord, Show)

-- | Parse `Callout` from blockquote blocks
parseCallout :: [B.Block] -> Maybe Callout
parseCallout = parseObsidianCallout

-- | Parse according to https://help.obsidian.md/Editing+and+formatting/Callouts
parseObsidianCallout :: [B.Block] -> Maybe Callout
parseObsidianCallout blks = do
  B.Para (B.Str firstStr : inlines) : body' <- pure blks
  let (headerText, restInlines) = absorbFoldSuffix firstStr inlines
  (type_, foldState) <- parseCalloutHeader headerText
  let (title', mFirstPara) = disrespectSoftbreak restInlines
      title = if null title' then defaultTitle type_ else title'
      body = maybe body' (: body') mFirstPara
  pure $ Callout {..}
  where
    defaultTitle :: CalloutType -> [B.Inline]
    defaultTitle t =
      let calloutTitle = toText $ Text.Casing.pascal $ toString $ unCalloutType t
       in [B.Str calloutTitle]

    -- Pandoc may tokenize @[!type]-@ either as a single @Str "[!type]-"@ or
    -- split across @Str "[!type]"@ followed by @Str "-"@. Absorb the latter
    -- case so the parser sees a single header string.
    --
    -- Note: we deliberately do not absorb a fold marker that appears after
    -- a 'B.Space' (i.e. @Str "[!type]"@, @Space@, @Str "-"@). In Obsidian
    -- markdown, @> [!tip] -@ means a tip callout whose title text starts
    -- with a hyphen, *not* a foldable callout — fold markers must be
    -- adjacent to the closing bracket.
    absorbFoldSuffix :: Text -> [B.Inline] -> (Text, [B.Inline])
    absorbFoldSuffix header (B.Str suffix : rest)
      | suffix == "+" || suffix == "-" = (header <> suffix, rest)
    absorbFoldSuffix header rest = (header, rest)

{- | If there is a `B.SoftBreak`, treat it as paragraph break.

We do this to support Obsidian callouts where the first paragraph can start
immediately after the callout heading without a newline break in between.
-}
disrespectSoftbreak :: [B.Inline] -> ([B.Inline], Maybe B.Block)
disrespectSoftbreak = \case
  [] -> ([], Nothing)
  (B.SoftBreak : rest) -> ([], Just (B.Para rest))
  (x : xs) ->
    let (a, b) = disrespectSoftbreak xs
     in (x : a, b)

-- | Parse, for example, "[!tip]" into 'Tip'.
parseCalloutType :: Text -> Maybe CalloutType
parseCalloutType = fmap fst . parseCalloutHeader

{- | Parse the callout header, e.g. @[!tip]@, @[!note]-@, or @[!warning]+@,
into the type and optional fold state.

Per https://help.obsidian.md/Editing+and+formatting/Callouts#Foldable+callouts,
a trailing @+@ marks a foldable callout that is initially expanded, while @-@
marks one that is initially collapsed.
-}
parseCalloutHeader :: Text -> Maybe (CalloutType, Maybe FoldState)
parseCalloutHeader =
  rightToMaybe . parse parser "<callout:header>"
  where
    parser :: M.Parsec Void Text (CalloutType, Maybe FoldState)
    parser = do
      void $ M.string "[!"
      s <- T.toLower . toText <$> M.some (M.alphaNumChar <|> M.char '-' <|> M.char '_' <|> M.char '/')
      void $ M.string "]"
      foldState <-
        M.optional
          $ M.choice
            [ Collapsed <$ M.char '-'
            , Expanded <$ M.char '+'
            ]
      typ <- maybe (fail "Unknown") pure $ parseType s
      pure (typ, foldState)
    parseType :: Text -> Maybe CalloutType
    parseType s' = do
      let s = T.strip s'
      guard $ not $ T.null s
      pure $ CalloutType s
    parse :: M.Parsec Void Text a -> String -> Text -> Either Text a
    parse p fn =
      first (toText . M.errorBundlePretty)
        . M.parse (p <* M.eof) fn
