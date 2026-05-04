module Emanote.Model.SDataSpec where

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Emanote.Model.SData qualified as SData
import Relude
import Test.Hspec

obj :: [Aeson.Pair] -> Aeson.Value
obj = Aeson.object

arr :: [Text] -> Aeson.Value
arr = Aeson.toJSON

spec :: Spec
spec = do
  describe "mergeAeson cascade semantics" $ do
    it "right wins on scalar conflict"
      $ SData.mergeAeson (obj ["name" .= ("old" :: Text)]) (obj ["name" .= ("new" :: Text)])
      `shouldBe` obj ["name" .= ("new" :: Text)]
    it "deep-merges objects by key"
      $ SData.mergeAeson
        (obj ["template" .= obj ["theme" .= ("blue" :: Text), "name" .= ("old" :: Text)]])
        (obj ["template" .= obj ["theme" .= ("green" :: Text)]])
      `shouldBe` obj ["template" .= obj ["theme" .= ("green" :: Text), "name" .= ("old" :: Text)]]
    it "concatenates arrays instead of aligning by index (issue #697)"
      $ SData.mergeAeson
        (obj ["tags" .= arr ["team-doc"]])
        (obj ["tags" .= arr ["internal-note"]])
      `shouldBe` obj ["tags" .= arr ["team-doc", "internal-note"]]
    it "deduplicates repeated array elements after concatenation"
      $ SData.mergeAeson
        (obj ["tags" .= arr ["team-doc", "shared"]])
        (obj ["tags" .= arr ["shared", "internal-note"]])
      `shouldBe` obj ["tags" .= arr ["team-doc", "shared", "internal-note"]]
    it "preserves single-side array values when only one side declares the key"
      $ SData.mergeAeson
        (obj ["tags" .= arr ["team-doc"]])
        (obj ["title" .= ("child" :: Text)])
      `shouldBe` obj ["tags" .= arr ["team-doc"], "title" .= ("child" :: Text)]
    it "carries array semantics through nested objects (e.g. pandoc.filters)"
      $ SData.mergeAeson
        (obj ["pandoc" .= obj ["filters" .= arr ["site-wide.lua"]]])
        (obj ["pandoc" .= obj ["filters" .= arr ["page-local.lua"]]])
      `shouldBe` obj ["pandoc" .= obj ["filters" .= arr ["site-wide.lua", "page-local.lua"]]]
  describe "mergeAesons folds left-to-right with right-most winning at scalars" $ do
    it "later cascade entries override earlier scalar values"
      $ SData.mergeAesons
        ( obj ["template" .= obj ["theme" .= ("blue" :: Text)]]
            :| [ obj ["template" .= obj ["theme" .= ("green" :: Text)]]
               , obj ["template" .= obj ["theme" .= ("red" :: Text)]]
               ]
        )
      `shouldBe` obj ["template" .= obj ["theme" .= ("red" :: Text)]]
    it "unions tags across the whole cascade"
      $ SData.mergeAesons
        ( obj ["tags" .= arr ["root"]]
            :| [ obj ["tags" .= arr ["mid"]]
               , obj ["tags" .= arr ["leaf"]]
               ]
        )
      `shouldBe` obj ["tags" .= arr ["root", "mid", "leaf"]]
