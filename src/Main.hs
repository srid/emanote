{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Lens.Operators ((.~))
import Data.Default (Default (def))
import qualified Ema
import qualified Emanote.Model as M
import qualified Emanote.Source as Source
import qualified Emanote.View.Template as Template
import qualified Heist.Extra.TemplateState as T
import Main.Utf8 (withUtf8)

main :: IO ()
main = do
  withUtf8 $
    Ema.runEma Template.render $ \_act m -> do
      emptyTmpl <- T.newTemplateState
      let initialModel = def & M.modelHeistTemplate .~ emptyTmpl
      Source.run m initialModel
