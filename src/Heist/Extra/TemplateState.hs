{-# LANGUAGE TypeApplications #-}

module Heist.Extra.TemplateState where

import Control.Lens.Operators ((.~))
import Control.Monad.Except (runExcept)
import Data.ByteString.Builder (toLazyByteString)
import qualified Heist as H
import qualified Heist.Interpreted as HI

type TemplateState = Either [String] (H.HeistState Identity)

defaultTemplateState :: TemplateState
defaultTemplateState = Left $ one "Heist state not yet loaded"

-- TODO: Eventually move these to Ema.Helper
-- Consider first the full practical range of template patterns,
-- https://softwaresimply.blogspot.com/2011/04/looping-and-control-flow-in-heist.html

loadHeistTemplates :: MonadIO m => FilePath -> m TemplateState
loadHeistTemplates templateDir = do
  -- TODO: Use heist compiled templates
  let heistCfg :: H.HeistConfig Identity =
        H.emptyHeistConfig
          & H.hcNamespace .~ ""
          & H.hcTemplateLocations .~ [H.loadTemplates templateDir]
  liftIO $ H.initHeist heistCfg

renderHeistTemplate ::
  ByteString ->
  H.Splices (HI.Splice Identity) ->
  TemplateState ->
  LByteString
renderHeistTemplate name splices etmpl =
  either error id . runExcept $ do
    heist <-
      hoistEither . first (unlines . fmap toText) $ etmpl
    (builder, _mimeType) <-
      tryJust "Unable to render" $
        runIdentity $ HI.renderTemplate (HI.bindSplices splices heist) name
    pure $ toLazyByteString builder
  where
    -- A 'fromJust' that fails in the 'ExceptT' monad
    tryJust :: Monad m => e -> Maybe a -> ExceptT e m a
    tryJust e m = hoistEither $ maybeToRight e m
