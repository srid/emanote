module Emanote.Pandoc.ExternalLink (
  setExternalLinkIcon,
) where

import Relude
import Text.Pandoc.Definition qualified as B
import Text.Pandoc.Walk qualified as W
import Text.Parsec qualified as P
import Text.Parsec.Char qualified as PC

-- Add a data-linkicon=external attribute to external links that contain some
-- text in their description, provided that they do not already have a
-- data-linkicon attribute.
setExternalLinkIcon :: W.Walkable B.Inline b => b -> b
setExternalLinkIcon =
  W.walk $ \case
    B.Link (id', classes, attrs) inlines (url, title)
      | hasURIScheme url && containsText inlines ->
          let showLinkIconAttr = ("data-linkicon", "external")
              newAttrs = insert attrs showLinkIconAttr
           in B.Link (id', classes, newAttrs) inlines (url, title)
    x -> x
  where
    -- Inserts an element in a key-value list if the element's key is not
    -- already in the list.
    insert :: Eq a => [(a, b)] -> (a, b) -> [(a, b)]
    insert as a
      | fst a `elem` (fst <$> as) = as
      | otherwise = a : as
    -- Checks whether the given text begins with an RFC 3986 compliant URI
    -- scheme.
    hasURIScheme :: Text -> Bool
    hasURIScheme =
      isRight . P.parse schemeP ""
      where
        schemeP = do
          c <- PC.letter
          cs <- P.many $ PC.alphaNum P.<|> P.oneOf ".-+"
          void $ PC.char ':'
          return (c : cs)
    -- Checks whether a list of inlines contains a (perhaps nested) "textual
    -- element", understood as a Pandoc `Str`, `Code` or `Math`.
    containsText :: [B.Inline] -> Bool
    containsText =
      getAny
        . W.query
          ( \case
              B.Str _ -> Any True
              B.Code _ _ -> Any True
              B.Math _ _ -> Any True
              _ -> Any False
          )
