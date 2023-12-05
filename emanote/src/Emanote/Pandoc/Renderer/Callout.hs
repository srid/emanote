{-# LANGUAGE RecordWildCards #-}

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
  callout <- parseCallout $ traceShowId blks
  pure $ do
    tpl <- traceShow (show callout) $ HE.lookupHtmlTemplateMust "/templates/filters/callout"
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
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance Default CalloutType where
  def = Note

data Callout = Callout
  { type_ :: CalloutType
  , title :: [B.Inline]
  , body :: [B.Block]
  }
  deriving stock (Eq, Ord, Show)

-- TODO: tests, for all cases

-- | Parse `Callout` from blockquote blocks
parseCallout :: [B.Block] -> Maybe Callout
parseCallout blks = do
  -- FIXME: handle SoftBreak continuation
  B.Para (B.Str calloutType : inlines) : body <- pure blks
  type_ <- parseCalloutType calloutType
  let title = case inlines of
        B.Space : tit -> tit
        _ -> defaultTitle type_
  pure $ Callout {..}

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
      s <- toText <$> M.some M.letterChar
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
