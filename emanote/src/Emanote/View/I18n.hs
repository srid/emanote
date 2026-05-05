module Emanote.View.I18n (
  selectedTranslations,
  i18nSplices,
  lookupText,
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

i18nSplices :: Aeson.Value -> H.Splices (HI.Splice Identity)
i18nSplices meta = do
  "ema:i18n" ## HJ.bindJson (Aeson.toJSON table)
  forM_ (Map.toList table) $ \(key, value) ->
    "ema:i18n:" <> key ## HI.textSplice value
  where
    table = selectedTranslations meta

languageFallbacks :: Text -> [Text]
languageFallbacks lang =
  ordNub $ filter (not . T.null) [lang, baseLang lang, "en"]
  where
    baseLang = T.takeWhile (\c -> c /= '-' && c /= '_')
