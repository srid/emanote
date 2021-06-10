module Heist.Extra.Splices.Pandoc.Attr where

import qualified Data.Text as T
import qualified Text.Pandoc.Definition as B

-- | Convert Pandoc attributes to XmlHtml attributes
rpAttr :: B.Attr -> [(Text, Text)]
rpAttr (id', classes, attrs) =
  let cls = T.intercalate " " classes
   in unlessNull id' [("id", id')]
        <> unlessNull cls [("class", cls)]
        <> mconcat (fmap (\(k, v) -> [(k, v)]) attrs)
  where
    unlessNull x f =
      if T.null x then mempty else f

-- | Merge two XmlHtml attributes set
addAttr :: B.Attr -> B.Attr -> B.Attr
addAttr (id1, cls1, attr1) (id2, cls2, attr2) =
  (pickNonNull id1 id2, cls1 <> cls2, attr1 <> attr2)
  where
    pickNonNull x "" = x
    pickNonNull "" x = x
    pickNonNull _ _ = ""
