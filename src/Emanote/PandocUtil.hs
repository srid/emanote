{-# LANGUAGE TypeApplications #-}

module Emanote.PandocUtil where

import qualified Ema.Helper.Markdown as Markdown
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))
import qualified Text.Pandoc.Walk as W

getPandocTitle :: Pandoc -> Maybe Text
getPandocTitle =
  fmap Markdown.plainify . getPandocH1

getPandocH1 :: Pandoc -> Maybe [B.Inline]
getPandocH1 = listToMaybe . W.query go
  where
    go :: B.Block -> [[B.Inline]]
    go = \case
      B.Header 1 _ inlines ->
        [inlines]
      _ ->
        []

withoutH1 :: Pandoc -> Pandoc
withoutH1 (Pandoc meta (B.Header 1 _ _ : rest)) =
  Pandoc meta rest
withoutH1 doc =
  doc

rewriteLinks :: (Text -> Text) -> Pandoc -> Pandoc
rewriteLinks f = runIdentity . rewriteLinksM (Identity . f)

rewriteLinksM :: Monad m => (Text -> m Text) -> Pandoc -> m Pandoc
rewriteLinksM f =
  W.walkM $ \case
    B.Link attr is (url, tit) -> do
      newUrl <- f url
      pure $ B.Link attr is (newUrl, tit)
    x -> pure x
