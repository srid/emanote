{-# LANGUAGE TypeApplications #-}

-- | Emanote.Source is responsible for managing the source files and its
-- changes, and communicating them to `Emanote.Model`.
module Emanote.Source
  ( emanate,
  )
where

import Control.Monad.Logger (MonadLogger)
import Data.LVar (LVar)
import qualified Data.LVar as LVar
import qualified Ema.Helper.FileSystem as EmaFS
import Emanote.Model (Model)
import Emanote.Prelude (log)
import Emanote.Source.Loc
import qualified Emanote.Source.Patch as Patch
import Emanote.Source.Pattern (filePatterns, ignorePatterns)
import Relude
import UnliftIO (MonadUnliftIO)

-- | Emanate on-disk sources onto an in-memory `Model` (stored in a LVar)
emanate :: (MonadUnliftIO m, MonadLogger m) => NonEmpty FilePath -> LVar Model -> Model -> m ()
emanate paths modelLvar initialModel = do
  defaultLayer <- liftIO emanoteDefaultLayer
  let layers = one defaultLayer <> userLayers paths
  mcmd <-
    EmaFS.unionMountOnLVar
      layers
      filePatterns
      ignorePatterns
      modelLvar
      initialModel
      Patch.mapFsChanges
  whenJust mcmd $ \EmaFS.Cmd_Remount -> do
    log "!! Remount suggested !!"
    LVar.set modelLvar initialModel -- Reset the model
    emanate paths modelLvar initialModel
