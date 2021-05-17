{-# LANGUAGE TypeApplications #-}

-- TODO: Eventually move `Heist.Extra` to Ema.Helper
-- Consider first the full practical range of template patterns,
-- https://softwaresimply.blogspot.com/2011/04/looping-and-control-flow-in-heist.html
module Heist.Extra.TemplateState
  ( TemplateState,
    loadHeistTemplates,
    renderHeistTemplate,
  )
where

import Control.Lens.Operators ((.~))
import Control.Monad.Except (runExcept)
import Data.ByteString.Builder (toLazyByteString)
import Data.Default (Default (..))
import qualified Heist as H
import qualified Heist.Interpreted as HI

newtype TemplateState = TemplateState {unTemplateState :: Either [String] (H.HeistState Identity)}

instance Default TemplateState where
  def = TemplateState $ Left $ one "Heist templates yet to be loaded"

loadHeistTemplates :: MonadIO m => FilePath -> m TemplateState
loadHeistTemplates templateDir = do
  -- TODO: Use heist compiled templates
  let heistCfg :: H.HeistConfig Identity =
        H.emptyHeistConfig
          & H.hcNamespace .~ ""
          & H.hcTemplateLocations .~ [H.loadTemplates templateDir]
  liftIO $ TemplateState <$> H.initHeist heistCfg

renderHeistTemplate ::
  Text ->
  H.Splices (HI.Splice Identity) ->
  TemplateState ->
  LByteString
renderHeistTemplate name splices st =
  either error id . runExcept $ do
    heist <-
      hoistEither . first (unlines . fmap toText) $ unTemplateState st
    (builder, _mimeType) <-
      tryJust ("Unable to render template '" <> name <> "'") $
        runIdentity $ HI.renderTemplate (HI.bindSplices splices heist) (encodeUtf8 name)
    pure $ toLazyByteString builder
  where
    -- A 'fromJust' that fails in the 'ExceptT' monad
    tryJust :: Monad m => e -> Maybe a -> ExceptT e m a
    tryJust e = hoistEither . maybeToRight e
