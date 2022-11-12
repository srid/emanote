module Emanote.Pandoc.BuiltinFiltersSpec where

import Emanote.Pandoc.BuiltinFilters
import Emanote.Pandoc.Markdown.Parser (parseMarkdown)
import Hedgehog
import Relude
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)
import Text.Pandoc.Definition (Inline (..))
import Text.Pandoc.Walk qualified as W

spec :: Spec
spec =
  do
    describe "setExternalLinkIcon" $ do
      let linkIconAttrs = fmap (W.query $ getLinkAttr "data-linkicon") . parseEmanoteMarkdown
          parseEmanoteMarkdown = fmap (preparePandoc . snd) . parseMarkdown "<test>"
      it "respects user-specified data-linkicon attribute" . hedgehog $ do
        linkIconAttrs "[test](https://www.test.com){data-linkicon=abc}" === Right ["abc"]
        linkIconAttrs "[test](https://www.test.com){data-linkicon=\"\"}" === Right [""]
        linkIconAttrs "[[foo]]{data-linkicon=external}" === Right ["external"]
        linkIconAttrs "[abc](x/y/z){data-linkicon=none}" === Right ["none"]
      it "does not add attribute if link is internal" . hedgehog $ do
        linkIconAttrs "[[foo]]" === Right []
        linkIconAttrs "[abc](x/y/z)" === Right []
        linkIconAttrs "![[image.jpg]]" === Right []
        linkIconAttrs "[![](path/to/image.png)](foo/bar/baz)" === Right []
        linkIconAttrs "[[bar|baz]]" === Right []
        linkIconAttrs "[`abc`](./test.txt)" === Right []
        linkIconAttrs "[$$abc$$](/just/test#ing)" === Right []
        linkIconAttrs "#tag" === Right []
        linkIconAttrs "[[abc|def]]" === Right []
      it "adds attribute if link is external and its description contains text, code or math" . hedgehog $ do
        linkIconAttrs "[Emanote](https://github.com/EmaApps/emanote)" === Right ["external"]
        linkIconAttrs "[`text`](http://somehost:1234/test)" === Right ["external"]
        linkIconAttrs "[$$E=mc^2$$](ssh://user@host.abc/~/path/)" === Right ["external"]
        linkIconAttrs "[![[picture.png]] ~~**`code`**~~](scheme://host:port/path?query)" === Right ["external"]
        linkIconAttrs "[==$$e^{i \\pi} + 1 = 0$$== ![[image.svg]]](git://host.xz/path/to/repo.git/)" === Right ["external"]
        linkIconAttrs "example@example.com" === Right ["external"]
        linkIconAttrs "https://www.test.gov" === Right ["external"]
        linkIconAttrs "[**![[image.jpg]] *qwerty* ![[image.jpg]]**](doi:10.1000/182)" === Right ["external"]
        linkIconAttrs "[:video_game:](bolo://hostname/)" === Right ["external"]
      it "does not add attribute if link description contains no text, code or math" . hedgehog $ do
        linkIconAttrs "[](http://nothing.interesting.here)" === Right []
        linkIconAttrs "[![[image.png]]](https://www.example.com)" === Right []
        linkIconAttrs "[==*_**~~![[img.png]]~~**_*==](http://something.info)" === Right []

getLinkAttr :: Text -> Inline -> [Text]
getLinkAttr name (Link (_, _, attrs) _ (_, _)) =
  snd <$> filter ((== name) . fst) attrs
getLinkAttr _ _ =
  []
