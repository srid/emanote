{-# OPTIONS_GHC -Wno-orphans #-}

module Emanote.Class where

import Ema (Ema (..))
import Emanote.Model (Model)
import qualified Emanote.Model as M
import Emanote.Route (MarkdownRoute)
import qualified Emanote.Route as R

instance Ema Model (Either FilePath MarkdownRoute) where
  encodeRoute =
    either id R.encodeRoute
  decodeRoute model fp =
    fmap Left (M.modelLookupStaticFile fp model)
      <|> fmap Right (R.decodeRoute fp)
  allRoutes = M.allRoutes
