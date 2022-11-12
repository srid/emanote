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
    describe "setExternalLinkIcon" $ do
      let linkIconAttrs = fmap (query getDataLinkIconAttr) . parseAndRunFilters
          parseAndRunFilters = fmap (preparePandoc . snd) . parseMarkdown ""
          getDataLinkIconAttr (Link (_, _, attrs) _ (_, _)) = snd <$> filter ((== "data-linkicon") . fst) attrs
          getDataLinkIconAttr _ = []
      it "respects user-specified data-linkicon attribute" . hedgehog $ do
        linkIconAttrs "[test](https://www.test.com){data-linkicon=abc}" === pure ["abc"]
        linkIconAttrs "[test](https://www.test.com){data-linkicon=\"\"}" === pure [""]
        linkIconAttrs "[[foo]]{data-linkicon=external}" === pure ["external"]
        linkIconAttrs "[abc](x/y/z){data-linkicon=none}" === pure ["none"]
      it "does not add attribute if link is internal" . hedgehog $ do
        linkIconAttrs "[[foo]]" === pure []
        linkIconAttrs "[abc](x/y/z)" === pure []
        linkIconAttrs "![[image.jpg]]" === pure []
        linkIconAttrs "[![](path/to/image.png)](foo/bar/baz)" === pure []
        linkIconAttrs "[[bar|baz]]" === pure []
        linkIconAttrs "[`abc`](./test.txt)" === pure []
        linkIconAttrs "[$$abc$$](/just/test#ing)" === pure []
        linkIconAttrs "#tag" === pure []
        linkIconAttrs "[[abc|def]]" === pure []
      it "adds attribute if link is external and its description contains text, code or math" . hedgehog $ do
        linkIconAttrs "[Emanote](https://github.com/EmaApps/emanote)" === pure ["external"]
        linkIconAttrs "[`text`](http://somehost:1234/test)" === pure ["external"]
        linkIconAttrs "[$$E=mc^2$$](ssh://user@host.abc/~/path/)" === pure ["external"]
        linkIconAttrs "[![[picture.png]] ~~**`code`**~~](scheme://host:port/path?query)" === pure ["external"]
        linkIconAttrs "[==$$e^{i \\pi} + 1 = 0$$== ![[image.svg]]](git://host.xz/path/to/repo.git/)" === pure ["external"]
        linkIconAttrs "example@example.com" === pure ["external"]
        linkIconAttrs "https://www.test.gov" === pure ["external"]
        linkIconAttrs "[**![[image.jpg]] *qwerty* ![[image.jpg]]**](doi:10.1000/182)" === pure ["external"]
        linkIconAttrs "[:video_game:](bolo://hostname/)" === pure ["external"]
      it "does not add attribute if link description contains no text, code or math" . hedgehog $ do
        linkIconAttrs "[](http://nothing.interesting.here)" === pure []
        linkIconAttrs "[![[image.png]]](https://www.example.com)" === pure []
        linkIconAttrs "[==*_**~~![[img.png]]~~**_*==](http://something.info)" === pure []
