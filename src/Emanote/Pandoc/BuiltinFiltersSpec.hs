module Emanote.Pandoc.BuiltinFiltersSpec where

import Emanote.Pandoc.BuiltinFilters
import Emanote.Pandoc.Markdown.Parser (parseMarkdown)
import Hedgehog
import Relude
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)
import Text.Pandoc.Definition (Inline (..), Pandoc)
import Text.Pandoc.Walk (query)

spec :: Spec
spec =
  do
    describe "markLinksWithIcon" $ do
      let runFilter =
            (>>=) . evalEither . fmap (query extractAttr) . applyFilter markLinksWithIcon
            where
              extractAttr (Link (_, _, attrs) _ (_, _)) = snd <$> filter ((== "data-linkicon") . fst) attrs
              extractAttr _ = []
      it "respects user-specified data-linkicon attribute" . hedgehog $ do
        runFilter "[test](https://www.test.com){data-linkicon=abc}" (=== ["abc"])
        runFilter "[test](https://www.test.com){data-linkicon=\"\"}" (=== [""])
        runFilter "[[foo]]{data-linkicon=show}" (=== ["show"])
        runFilter "[abc](x/y/z){data-linkicon=hide}" (=== ["hide"])
      it "does not add attribute if link is internal" . hedgehog $ do
        runFilter "[[foo]]" (=== [])
        runFilter "[abc](x/y/z)" (=== [])
        runFilter "![[image.jpg]]" (=== [])
        runFilter "[![](path/to/image.png)](foo/bar/baz)" (=== [])
        runFilter "[[bar|baz]]" (=== [])
        runFilter "[`abc`](./test.txt)" (=== [])
        runFilter "[$$abc$$](/just/test#ing)" (=== [])
      it "adds attribute if link is external and its description contains text, code or math" . hedgehog $ do
        runFilter "[Emanote](https://github.com/EmaApps/emanote)" (=== ["show"])
        runFilter "[`text`](http://somehost:1234/test)" (=== ["show"])
        runFilter "[$$E=mc^2$$](ssh://user@host.abc/~/path/)" (=== ["show"])
        runFilter "[![[picture.png]] ~~**`code`**~~](scheme://host:port/path?query)" (=== ["show"])
        runFilter "[==$$e^{i \\pi} + 1 = 0$$== ![[image.svg]]](git://host.xz/path/to/repo.git/)" (=== ["show"])
        runFilter "example@example.com" (=== ["show"])
        runFilter "https://www.test.gov" (=== ["show"])
        runFilter "[**![[image.jpg]] *qwerty* ![[image.jpg]]**](doi:10.1000/182)" (=== ["show"])
        runFilter "[:video_game:](bolo://hostname/)" (=== ["show"])
      it "does not add attribute if link description contains no text, code or math" . hedgehog $ do
        runFilter "[](http://nothing.interesting.here)" (=== [])
        runFilter "[![[image.png]]](https://www.example.com)" (=== [])
        runFilter "[==*_**~~![[img.png]]~~**_*==](http://something.info)" (=== [])

applyFilter :: (Pandoc -> Pandoc) -> Text -> Either Text Pandoc
applyFilter f = fmap (f . snd) . parseMarkdown ""
