module Emanote.View.I18n (
  selectedTranslations,
  i18nSplices,
  lookupText,
  lookupTextWith,
) where

import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Map.Syntax ((##))
import Data.Text qualified as T
import Emanote.Model.SData qualified as SData
import Heist qualified as H
import Heist.Interpreted qualified as HI
import Heist.Splices.Json qualified as HJ
import Relude
import Text.XmlHtml qualified as X

type TranslationTable = Map Text (Map Text Text)

selectedTranslations :: Aeson.Value -> Map Text Text
selectedTranslations meta =
  Map.unions $ mapMaybe (`Map.lookup` tables) (languageFallbacks lang)
  where
    lang = SData.lookupAeson @Text "en" ("page" :| ["lang"]) meta
    tables = SData.lookupAeson @TranslationTable mempty ("template" :| ["i18n"]) meta

lookupText :: Aeson.Value -> Text -> Text -> Text
lookupText meta key fallback =
  Map.findWithDefault fallback key (selectedTranslations meta)

lookupTextWith :: Aeson.Value -> Text -> Map Text Text -> Text -> Text
lookupTextWith meta key params fallback =
  replaceParams params $ lookupText meta key fallback

i18nSplices :: Aeson.Value -> H.Splices (HI.Splice Identity)
i18nSplices meta = do
  "ema:i18n" ## HJ.bindJson (Aeson.toJSON table)
  "ema:i18n:json" ## pure [X.TextNode $ jsonScriptText table]
  "ema:i18n:rich" ## richTextSplice table
  forM_ (Map.toList table) $ \(key, value) ->
    "ema:i18n:" <> key ## HI.textSplice value
  where
    table = selectedTranslations meta

richTextSplice :: Map Text Text -> HI.Splice Identity
richTextSplice table = do
  node <- H.getParamNode
  let key =
        fromMaybe (error "<ema:i18n:rich> missing 'key' attribute")
          $ X.getAttribute "key" node
      message = Map.findWithDefault "" key table
  slot <- HI.runNodeList (X.elementChildren node)
  pure $ slotMessage "{link}" slot message

slotMessage :: Text -> [X.Node] -> Text -> [X.Node]
slotMessage placeholder slot message =
  case T.splitOn placeholder message of
    [] -> []
    firstPart : rest ->
      X.TextNode firstPart : concatMap (\part -> slot <> [X.TextNode part]) rest

replaceParams :: Map Text Text -> Text -> Text
replaceParams params template =
  foldl' replaceOne template (Map.toList params)
  where
    replaceOne acc (name, value) =
      T.replace ("{" <> name <> "}") value acc

jsonScriptText :: (Aeson.ToJSON a) => a -> Text
jsonScriptText =
  T.replace "&" "\\u0026"
    . T.replace ">" "\\u003e"
    . T.replace "<" "\\u003c"
    . decodeUtf8
    . Aeson.encode

languageFallbacks :: Text -> [Text]
languageFallbacks lang =
  ordNub $ filter (not . T.null) [lang, baseLang lang, "en"]
  where
    baseLang = T.takeWhile (\c -> c /= '-' && c /= '_')
