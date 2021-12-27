{-# LANGUAGE TypeApplications #-}

module Emanote
  ( emanate,
    ChangeHandler,
  )
where

import Control.Monad.Logger (MonadLogger)
import Data.LVar (LVar)
import qualified Data.LVar as LVar
import qualified Data.Map.Strict as Map
import qualified Ema.Helper.FileSystem as EmaFS
import Emanote.Prelude (chainM, log)
import Emanote.Source.Loc (Loc)
import Relude
import System.FilePattern (FilePattern)
import UnliftIO (BufferMode (..), MonadUnliftIO, hSetBuffering)
import UnliftIO.IO (hFlush)

type ChangeHandler tag model m = tag -> FilePath -> EmaFS.FileAction (NonEmpty (Loc, FilePath)) -> m (model -> model)

-- | Emanate on-disk sources onto an in-memory `model` (stored in a LVar)
--
-- This is a generic extension to unionMountOnLVar for operating Emanote like
-- apps.
emanate ::
  (MonadUnliftIO m, MonadLogger m, Ord tag) =>
  -- Layers to mount
  Set (Loc, FilePath) ->
  [(tag, FilePattern)] ->
  -- | Ignore patterns
  [FilePattern] ->
  LVar model ->
  model ->
  ChangeHandler tag model m ->
  m ()
emanate layers filePatterns ignorePatterns modelLvar initialModel f = do
  mcmd <-
    EmaFS.unionMountOnLVar
      layers
      filePatterns
      ignorePatterns
      modelLvar
      initialModel
      (mapFsChanges f)
  whenJust mcmd $ \EmaFS.Cmd_Remount -> do
    log "!! Remount suggested !!"
    LVar.set modelLvar initialModel -- Reset the model
    emanate layers filePatterns ignorePatterns modelLvar initialModel f

mapFsChanges :: (MonadIO m, MonadLogger m) => ChangeHandler tag model m -> EmaFS.Change Loc tag -> m (model -> model)
mapFsChanges h ch = do
  withBlockBuffering $
    uncurry (mapFsChangesOnExt h) `chainM` Map.toList ch
  where
    -- Temporarily use block buffering before calling an IO action that is
    -- known ahead to log rapidly, so as to not hamper serial processing speed.
    withBlockBuffering f =
      hSetBuffering stdout (BlockBuffering Nothing)
        *> f
        <* (hSetBuffering stdout LineBuffering >> hFlush stdout)

mapFsChangesOnExt ::
  (MonadIO m, MonadLogger m) =>
  ChangeHandler tag model m ->
  tag ->
  Map FilePath (EmaFS.FileAction (NonEmpty (Loc, FilePath))) ->
  m (model -> model)
mapFsChangesOnExt h fpType fps = do
  uncurry (h fpType) `chainM` Map.toList fps
