{-# LANGUAGE RecordWildCards #-}

module Emanote.Pandoc.Renderer.Callout (
  calloutResolvingSplice,
) where

import Data.Map.Syntax ((##))
import Emanote.Model (Model)
import Emanote.Model.Title qualified as Tit
import Emanote.Pandoc.Renderer (PandocBlockRenderer)
import Emanote.Route (LMLRoute)
import Heist.Extra qualified as HE
import Heist.Extra.Splices.Pandoc qualified as HP
import Heist.Interpreted qualified as HI
import Relude
import Text.Pandoc.Definition qualified as B

data CalloutType
  = -- Default callout type
    Note
  | Tip
  | Warning
  | Failure
  deriving stock (Eq, Ord, Show, Enum, Bounded)

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
        _ -> [B.Str $ show type_]
  pure $ Callout {..}

-- | Parse, for example, "[!tip]" into 'Tip'.
parseCalloutType :: Text -> Maybe CalloutType
parseCalloutType = \case
  -- TODO: refactor, and finish
  "[!note]" -> Just Note
  "[!tip]" -> Just Tip
  "[!warning]" -> Just Warning
  "[!failure]" -> Just Failure
  _ -> Nothing

calloutResolvingSplice :: PandocBlockRenderer Model LMLRoute
calloutResolvingSplice _model _nr ctx _noteRoute blk = do
  B.BlockQuote blks <- pure blk
  callout <- parseCallout $ traceShowId blks
  pure $ do
    tpl <- traceShow (show callout) $ HE.lookupHtmlTemplateMust "/templates/filters/callout"
    HE.runCustomTemplate tpl $ do
      "callout:type" ## HI.textSplice (show $ type_ callout)
      "callout:title" ## Tit.titleSplice ctx id $ Tit.fromInlines (title callout)
      "callout:body" ## HP.pandocSplice ctx $ B.Pandoc mempty (body callout)
      "query" ##
        HI.textSplice (show blks)
