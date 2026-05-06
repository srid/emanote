module Emanote.Source.IgnoreSpec where

import Emanote.Source.Ignore (parsePatterns)
import Relude
import Test.Hspec

spec :: Spec
spec = do
  describe "parsePatterns" $ do
    it "returns one pattern per non-empty line"
      $ parsePatterns "node_modules/**\nflake.lock\n"
      `shouldBe` ["node_modules/**", "flake.lock"]

    it "skips blank lines and lines starting with #"
      $ parsePatterns
        ( unlines
            [ "# top-level comment"
            , ""
            , "templates/*.md"
            , "  "
            , "# trailing comment"
            , "AGENTS.md"
            ]
        )
      `shouldBe` ["templates/*.md", "AGENTS.md"]

    it "trims surrounding whitespace from each pattern"
      $ parsePatterns "  drafts/**  \n\tnotes/private.md\n"
      `shouldBe` ["drafts/**", "notes/private.md"]

    it "treats # only as a line-leading marker, not mid-line"
      $
      -- A `#` inside a pattern is a literal character. Only lines whose
      -- first non-whitespace character is `#` are treated as comments.
      parsePatterns "channel#drafts/**\n"
      `shouldBe` ["channel#drafts/**"]
