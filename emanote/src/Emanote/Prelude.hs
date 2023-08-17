{-# LANGUAGE DeriveAnyClass #-}

{- | We use `relude` as our prelude; any extra Prelude-like functionality is put
 here.
-}
module Emanote.Prelude where

import Control.Monad.Logger (MonadLogger, logDebugNS, logErrorNS, logInfoNS, logWarnNS)
import Relude

-- | Monadic version of `chain`
chainM :: (Monad m) => (b -> m (a -> a)) -> [b] -> m (a -> a)
chainM f =
  fmap chain . mapM f
  where
    -- Apply the list of actions in the given order to an initial argument.
    --
    -- chain [f1, f2, ...] a = ... (f2 (f1 x))
    chain :: [a -> a] -> a -> a
    chain = flip $ flipfoldl' ($)

-- | User-provided input is malformed.
newtype BadInput = BadInput Text
  deriving stock (Show)
  deriving anyclass (Exception)

-------------
-- Logging
--------------

log :: (MonadLogger m) => Text -> m ()
log = logInfoNS "emanote"

logD :: (MonadLogger m) => Text -> m ()
logD = logDebugNS "emanote"

logE :: (MonadLogger m) => Text -> m ()
logE = logErrorNS "emanote"

logW :: (MonadLogger m) => Text -> m ()
logW = logWarnNS "emanote"
