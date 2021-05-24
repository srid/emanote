module Emanote.Logging where

import Control.Monad.Logger (MonadLogger, logDebugNS, logErrorNS, logInfoNS)

log :: MonadLogger m => Text -> m ()
log = logInfoNS "emanote"

logD :: MonadLogger m => Text -> m ()
logD = logDebugNS "emanote"

logE :: MonadLogger m => Text -> m ()
logE = logErrorNS "emanote"
