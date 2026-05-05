module Emanote.View.I18nSpec where

import Data.Aeson qualified as Aeson
import Emanote.View.I18n (lookupText, selectedTranslations)
import Relude
import Test.Hspec

spec :: Spec
spec = do
  describe "selectedTranslations" $ do
    it "selects the page language over English" $ do
      lookupText frenchMeta "home" "fallback" `shouldBe` "Accueil"

    it "falls back through the base language for regional tags" $ do
      lookupText frenchCanadianMeta "home" "fallback" `shouldBe` "Accueil"

    it "keeps English entries when the selected language is incomplete" $ do
      lookupText frenchMeta "tasks" "fallback" `shouldBe` "Tasks"

    it "falls back to English for unknown languages" $ do
      lookupText unknownLangMeta "home" "fallback" `shouldBe` "Home"

    it "uses the caller fallback when no translation table has the key" $ do
      lookupText frenchMeta "missing" "fallback" `shouldBe` "fallback"

    it "exposes the merged selected table" $ do
      selectedTranslations frenchMeta `shouldBe` fromList [("home", "Accueil"), ("tasks", "Tasks")]

frenchMeta :: Aeson.Value
frenchMeta =
  metaWithLang "fr"

frenchCanadianMeta :: Aeson.Value
frenchCanadianMeta =
  metaWithLang "fr-CA"

unknownLangMeta :: Aeson.Value
unknownLangMeta =
  metaWithLang "zz"

metaWithLang :: Text -> Aeson.Value
metaWithLang lang =
  Aeson.object
    [ "page" Aeson..= Aeson.object ["lang" Aeson..= lang]
    , "template"
        Aeson..= Aeson.object
          [ "i18n"
              Aeson..= Aeson.object
                [ "en" Aeson..= Aeson.object ["home" Aeson..= ("Home" :: Text), "tasks" Aeson..= ("Tasks" :: Text)]
                , "fr" Aeson..= Aeson.object ["home" Aeson..= ("Accueil" :: Text)]
                ]
          ]
    ]
