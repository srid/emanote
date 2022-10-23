module Emanote.Pandoc.BuiltinFiltersSpec where

import Emanote.Pandoc.BuiltinFilters
import Emanote.Pandoc.Markdown.Parser (parseMarkdown)
import Hedgehog
import Relude
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)
import Text.Pandoc.Definition (Inline (..))
import Text.Pandoc.Walk (query)

spec :: Spec
spec =
  do
    describe "setExternalLinkicon" $ do
      let run =
            (>>=) . evalEither . fmap (query extractAttr . preparePandoc . snd) . parseMarkdown ""
            where
              extractAttr (Link (_, _, attrs) _ (_, _)) = snd <$> filter ((== "data-linkicon") . fst) attrs
              extractAttr _ = []
      it "respects user-specified data-linkicon attribute" . hedgehog $ do
        run "[test](https://www.test.com){data-linkicon=abc}" (=== ["abc"])
        run "[test](https://www.test.com){data-linkicon=\"\"}" (=== [""])
        run "[[foo]]{data-linkicon=external}" (=== ["external"])
        run "[abc](x/y/z){data-linkicon=none}" (=== ["none"])
      it "does not add attribute if link is internal" . hedgehog $ do
        run "[[foo]]" (=== [])
        run "[abc](x/y/z)" (=== [])
        run "![[image.jpg]]" (=== [])
        run "[![](path/to/image.png)](foo/bar/baz)" (=== [])
        run "[[bar|baz]]" (=== [])
        run "[`abc`](./test.txt)" (=== [])
        run "[$$abc$$](/just/test#ing)" (=== [])
        run "#tag" (=== [])
      it "adds attribute if link is external and its description contains text, code or math" . hedgehog $ do
        run "[Emanote](https://github.com/EmaApps/emanote)" (=== ["external"])
        run "[`text`](http://somehost:1234/test)" (=== ["external"])
        run "[$$E=mc^2$$](ssh://user@host.abc/~/path/)" (=== ["external"])
        run "[![[picture.png]] ~~**`code`**~~](scheme://host:port/path?query)" (=== ["external"])
        run "[==$$e^{i \\pi} + 1 = 0$$== ![[image.svg]]](git://host.xz/path/to/repo.git/)" (=== ["external"])
        run "example@example.com" (=== ["external"])
        run "https://www.test.gov" (=== ["external"])
        run "[**![[image.jpg]] *qwerty* ![[image.jpg]]**](doi:10.1000/182)" (=== ["external"])
        run "[:video_game:](bolo://hostname/)" (=== ["external"])
      it "does not add attribute if link description contains no text, code or math" . hedgehog $ do
        run "[](http://nothing.interesting.here)" (=== [])
        run "[![[image.png]]](https://www.example.com)" (=== [])
        run "[==*_**~~![[img.png]]~~**_*==](http://something.info)" (=== [])
