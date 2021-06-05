{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Ema
import qualified Emanote.Model as Model
import qualified Emanote.Source as Source
import qualified Emanote.View as View
import Main.Utf8 (withUtf8)

main :: IO ()
main =
  withUtf8 $
    Ema.runEma View.render $ \_act m ->
      Source.emanate m =<< Model.emptyModel
