{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | We use `relude` as our prelude; any extra Prelude-like functionality is put
-- here.
module Emanote.Prelude where

import Control.Monad.Logger (MonadLogger, logDebugNS, logErrorNS, logInfoNS)
import Data.WorldPeace.Union
  ( ElemRemove,
    OpenUnion,
    Remove,
    openUnionHandle,
  )

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

-- OpenUnion

-- Just an alias to avoid having to write this repeatedly.
h :: forall a (as :: [*]) b. ElemRemove a as => (OpenUnion (Remove a as) -> b) -> (a -> b) -> OpenUnion as -> b
h = openUnionHandle
