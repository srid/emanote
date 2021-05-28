{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | We use `relude` as our prelude; any extra Prelude-like functionality is put
-- here.
module Emanote.Prelude where

import Control.Monad.Logger (MonadLogger, logDebugNS, logErrorNS, logInfoNS)
import qualified Ema.Helper.Markdown as Markdown
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))
import qualified Text.Pandoc.Walk as W

-- | Monadic version of `chain`
chainM :: Monad m => (b -> m (a -> a)) -> [b] -> m (a -> a)
chainM f =
  fmap chain . mapM f
  where
    -- Apply the list of actions in the given order to an initial argument.
    --
    -- chain [f1, f2, ...] a = ... (f2 (f1 x))
    chain :: [a -> a] -> a -> a
    chain = flip $ foldl' $ flip ($)

-- | User-provided input is malformed.
newtype BadInput = BadInput Text
  deriving (Show, Exception)

-------------
-- Logging
--------------

log :: MonadLogger m => Text -> m ()
log = logInfoNS "emanote"

logD :: MonadLogger m => Text -> m ()
logD = logDebugNS "emanote"

logE :: MonadLogger m => Text -> m ()
logE = logErrorNS "emanote"

--------------------------
-- Pandoc prelude
---------------------------

getPandocTitle :: Pandoc -> Maybe Text
getPandocTitle =
  fmap Markdown.plainify . getPandocH1

getPandocH1 :: Pandoc -> Maybe [B.Inline]
getPandocH1 (Pandoc _ (B.Header 1 _ inlines : _rest)) =
  Just inlines
getPandocH1 _ =
  Nothing

withoutH1 :: Pandoc -> Pandoc
withoutH1 (Pandoc meta (B.Header 1 _ _ : rest)) =
  Pandoc meta rest
withoutH1 doc =
  doc

-- TODO: Should we consolidate this with PandocSplice behaviour, in an uniform way?
rewriteLinks ::
  ([(Text, Text)] -> ([B.Inline], Text) -> Either Text ([B.Inline], Text)) ->
  Pandoc ->
  Pandoc
rewriteLinks f =
  W.walk $ \case
    x@(B.Link attr@(_id, _class, otherAttrs) is (url, tit)) -> do
      case f (otherAttrs <> one ("title", tit)) (is, url) of
        Left err ->
          B.Span ("", one "emanote:broken-link", one ("title", err)) (one x)
        Right (newIs, newUrl) ->
          B.Link attr newIs (newUrl, tit)
    x -> x
