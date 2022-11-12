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
spec = do
  describe "setExternalLinkIcon" $ do
    it "respects user-specified data-linkicon attribute" . hedgehog $ do
      getDataLinkIconAttrs "[test](https://www.test.com){data-linkicon=abc}" === Right ["abc"]
      getDataLinkIconAttrs "[test](https://www.test.com){data-linkicon=\"\"}" === Right [""]
      getDataLinkIconAttrs "[[foo]]{data-linkicon=external}" === Right ["external"]
      getDataLinkIconAttrs "[abc](x/y/z){data-linkicon=none}" === Right ["none"]
    it "does not add attribute if link is internal" . hedgehog $ do
      getDataLinkIconAttrs "[[foo]]" === Right []
      getDataLinkIconAttrs "[abc](x/y/z)" === Right []
      getDataLinkIconAttrs "![[image.jpg]]" === Right []
      getDataLinkIconAttrs "[![](path/to/image.png)](foo/bar/baz)" === Right []
      getDataLinkIconAttrs "[[bar|baz]]" === Right []
      getDataLinkIconAttrs "[`abc`](./test.txt)" === Right []
      getDataLinkIconAttrs "[$$abc$$](/just/test#ing)" === Right []
      getDataLinkIconAttrs "#tag" === Right []
      getDataLinkIconAttrs "[[abc|def]]" === Right []
    it "adds attribute if link is external and its description contains text, code or math" . hedgehog $ do
      getDataLinkIconAttrs "[Emanote](https://github.com/EmaApps/emanote)" === Right ["external"]
      getDataLinkIconAttrs "[`text`](http://somehost:1234/test)" === Right ["external"]
      getDataLinkIconAttrs "[$$E=mc^2$$](ssh://user@host.abc/~/path/)" === Right ["external"]
      getDataLinkIconAttrs "[![[picture.png]] ~~**`code`**~~](scheme://host:port/path?query)" === Right ["external"]
      getDataLinkIconAttrs "[==$$e^{i \\pi} + 1 = 0$$== ![[image.svg]]](git://host.xz/path/to/repo.git/)" === Right ["external"]
      getDataLinkIconAttrs "example@example.com" === Right ["external"]
      getDataLinkIconAttrs "https://www.test.gov" === Right ["external"]
      getDataLinkIconAttrs "[**![[image.jpg]] *qwerty* ![[image.jpg]]**](doi:10.1000/182)" === Right ["external"]
      getDataLinkIconAttrs "[:video_game:](bolo://hostname/)" === Right ["external"]
    it "does not add attribute if link description contains no text, code or math" . hedgehog $ do
      getDataLinkIconAttrs "[](http://nothing.interesting.here)" === Right []
      getDataLinkIconAttrs "[![[image.png]]](https://www.example.com)" === Right []
      getDataLinkIconAttrs "[==*_**~~![[img.png]]~~**_*==](http://something.info)" === Right []

-- | Extract "data-linkicon" attributes present in the given Markdown content.
getDataLinkIconAttrs :: Text -> Either Text [Text]
getDataLinkIconAttrs =
  fmap (W.query $ getLinkAttr "data-linkicon") . parseEmanoteMarkdown
  where
    parseEmanoteMarkdown = fmap (preparePandoc . snd) . parseMarkdown "<test>"

getLinkAttr :: Text -> Inline -> [Text]
getLinkAttr name (Link (_, _, attrs) _ (_, _)) =
  snd <$> filter ((== name) . fst) attrs
getLinkAttr _ _ =
  []
