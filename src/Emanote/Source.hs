{-# LANGUAGE TypeApplications #-}

-- | Emanote.Source is responsible for managing the source files and its
-- changes, and communicating them to `Emanote.Model`.
module Emanote.Source
  ( run,
  )
where

import Control.Monad.Logger (MonadLogger)
import Data.LVar (LVar)
import Emanote.Model (Model)
import Emanote.Source.Loc (locLayers)
import qualified Emanote.Source.Mount as Mount
import Emanote.Source.Patch (transformActions)
import Emanote.Source.Pattern (filePatterns, ignorePatterns)
import UnliftIO (MonadUnliftIO)

run :: (MonadUnliftIO m, MonadLogger m) => LVar Model -> Model -> m ()
run modelLvar initialModel = do
  fsLayers <- liftIO locLayers
  Mount.unionMountOnLVar
    fsLayers
    filePatterns
    ignorePatterns
    modelLvar
    initialModel
    transformActions
