{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Ema
import qualified Emanote.Model as Model
import qualified Emanote.Source as Source
import qualified Emanote.View as View
import Main.Utf8 (withUtf8)
import qualified Emanote.CLI as CLI

main :: IO ()
main =
  withUtf8 $ do 
    cli <- CLI.parseCli
    Ema.runEmaWithCli (CLI.emaCli cli) View.render $ \_act m ->
      Source.emanate (CLI.layers cli) m =<< Model.emptyModel
