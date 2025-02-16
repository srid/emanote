{-# LANGUAGE RecordWildCards #-}

{- |  Obsidian-style callouts

  TODO: Should we switch to using the commonmark-hs parser here? cf. https://github.com/jgm/commonmark-hs/pull/135
-}
module Emanote.Pandoc.Renderer.Callout (
  calloutResolvingSplice,

  -- * For tests
  CalloutType (..),
  Callout (..),
  parseCalloutType,
) where

import Control.Monad (msum)
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
import Relude
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.Pandoc.Definition qualified as B

calloutResolvingSplice :: PandocBlockRenderer Model LMLRoute
calloutResolvingSplice _model _nr ctx _noteRoute blk = do
  B.BlockQuote blks <- pure blk
  callout <- parseCallout blks
  pure $ do
    tpl <- HE.lookupHtmlTemplateMust "/templates/filters/callout"
    HE.runCustomTemplate tpl $ do
      "callout:type" ## HI.textSplice (T.toLower $ show $ type_ callout)
      "callout:title" ## Tit.titleSplice ctx id $ Tit.fromInlines (title callout)
      "callout:body" ## HP.pandocSplice ctx $ B.Pandoc mempty (body callout)
      "query" ##
        HI.textSplice (show blks)

{- | Obsidian callout type

TODO: Add the rest, from https://help.obsidian.md/Editing+and+formatting/Callouts#Supported%20types
-}
data CalloutType
  = Note
  | Info
  | Tip
  | Warning
  | Failure
  | Quote
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance Default CalloutType where
  def = Note

data Callout = Callout
  { type_ :: CalloutType
  , title :: [B.Inline]
  , body :: [B.Block]
  }
  deriving stock (Eq, Ord, Show)

-- | Parse `Callout` from blockquote blocks
parseCallout :: [B.Block] -> Maybe Callout
parseCallout = parseObsidianCallout

-- | Parse according to https://help.obsidian.md/Editing+and+formatting/Callouts
parseObsidianCallout :: [B.Block] -> Maybe Callout
parseObsidianCallout blks = do
  B.Para (B.Str calloutType : inlines) : body' <- pure blks
  type_ <- parseCalloutType calloutType
  let (title', mFirstPara) = disrespectSoftbreak inlines
      title = if null title' then defaultTitle type_ else title'
      body = maybe body' (: body') mFirstPara
  pure $ Callout {..}

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

defaultTitle :: CalloutType -> [B.Inline]
defaultTitle t =
  [B.Str $ show t]

-- | Parse, for example, "[!tip]" into 'Tip'.
parseCalloutType :: Text -> Maybe CalloutType
parseCalloutType =
  rightToMaybe . parse parser "<callout:type>"
  where
    parser :: M.Parsec Void Text CalloutType
    parser = do
      void $ M.string "[!"
      s <- T.toLower . toText <$> M.some M.letterChar
      void $ M.string "]"
      maybe (fail "Unknown") pure $ parseType s
    parseType :: Text -> Maybe CalloutType
    parseType s =
      msum $ flip fmap (universe @CalloutType) $ \t -> do
        guard $ s == T.toLower (show @Text t)
        Just t
    parse :: M.Parsec Void Text a -> String -> Text -> Either Text a
    parse p fn =
      first (toText . M.errorBundlePretty)
        . M.parse (p <* M.eof) fn
