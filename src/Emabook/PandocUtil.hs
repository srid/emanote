{-# LANGUAGE TypeApplications #-}

module Emabook.PandocUtil where

import qualified Data.Text as T
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

rewriteRelativeLinks :: (Text -> Text) -> Pandoc -> Pandoc
rewriteRelativeLinks f =
  runIdentity . rewriteRelativeLinksM @Identity (Identity . f)

rewriteRelativeLinksM :: Monad m => (Text -> m Text) -> Pandoc -> m Pandoc
rewriteRelativeLinksM f =
  rewriteLinksM $ \url -> do
    if "://" `T.isInfixOf` url
      then pure url
      else f url

rewriteLinksM :: Monad m => (Text -> m Text) -> Pandoc -> m Pandoc
rewriteLinksM f =
  W.walkM $ \case
    B.Link attr is (url, tit) -> do
      newUrl <- f url
      pure $ B.Link attr is (newUrl, tit)
    x -> pure x
