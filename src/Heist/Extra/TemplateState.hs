-- TODO: Eventually move `Heist.Extra` to Ema.Helper
-- Consider first the full practical range of template patterns,
-- https://softwaresimply.blogspot.com/2011/04/looping-and-control-flow-in-heist.html
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Heist.Extra.TemplateState
  ( TemplateState,
    newTemplateState,
    loadHeistTemplates,
    addTemplateFile,
    renderHeistTemplate,
  )
where

import Control.Lens.Operators ((.~))
import Control.Monad.Except (runExcept)
import Data.ByteString.Builder (toLazyByteString)
import Data.Default (Default (..))
import qualified Data.Text as T
import qualified Heist as H
import qualified Heist.Interpreted as HI
import System.FilePath (splitExtension)
import Text.Show (Show (..))
import qualified Text.XmlHtml as XmlHtml

newtype TemplateState = TemplateState {unTemplateState :: Either [String] (H.HeistState Identity)}

instance Default TemplateState where
  def = TemplateState $ Left $ one "Heist templates yet to be loaded"

instance Show TemplateState where
  show = \case
    TemplateState (Left errs) ->
      "Heist errors: \n" <> toString (unlines (toText <$> errs))
    TemplateState (Right st) ->
      let names :: [Text] = sort $ H.templateNames st <&> T.intercalate "/" . reverse . fmap (decodeUtf8 @Text)
       in "Heist templates: " <> toString (T.intercalate ", " names)

newTemplateState :: MonadIO m => m TemplateState
newTemplateState = do
  -- TODO: Use heist compiled templates
  let heistCfg :: H.HeistConfig Identity =
        H.emptyHeistConfig
          & H.hcNamespace .~ ""
  -- & H.hcTemplateLocations .~ [pure $ Left $ one "Uninitialized"]
  liftIO $ TemplateState <$> H.initHeist heistCfg

addTemplateFile :: HasCallStack => FilePath -> ByteString -> TemplateState -> TemplateState
addTemplateFile fp s (TemplateState eSt) =
  TemplateState $ do
    st <- eSt
    first one (XmlHtml.parseHTML fp s) >>= \case
      XmlHtml.XmlDocument {} ->
        Left $ one "Xml unsupported"
      XmlHtml.HtmlDocument {..} -> do
        Right $ HI.addTemplate tmplName docContent (Just fp) st
  where
    tmplName = fromMaybe (error "Not a .tpl file") $ do
      let (base, ext) = splitExtension fp
      guard $ ext == ".tpl"
      pure $ encodeUtf8 base

loadHeistTemplates :: MonadIO m => FilePath -> m TemplateState
loadHeistTemplates templateDir = do
  -- TODO: Use heist compiled templates
  let eRepo = H.loadTemplates templateDir
      heistCfg :: H.HeistConfig Identity =
        H.emptyHeistConfig
          & H.hcNamespace .~ ""
          & H.hcTemplateLocations .~ [eRepo]
  liftIO $ TemplateState <$> H.initHeist heistCfg

renderHeistTemplate ::
  HasCallStack =>
  Text ->
  H.Splices (HI.Splice Identity) ->
  TemplateState ->
  LByteString
renderHeistTemplate name splices st =
  either error id . runExcept $ do
    heist <-
      hoistEither . first (unlines . fmap toText) $ unTemplateState st
    (builder, _mimeType) <-
      tryJust ("Unable to render template '" <> name <> "' -- " <> Prelude.show st) $
        runIdentity $ HI.renderTemplate (HI.bindSplices splices heist) (encodeUtf8 name)
    pure $ toLazyByteString builder
  where
    -- A 'fromJust' that fails in the 'ExceptT' monad
    tryJust :: Monad m => e -> Maybe a -> ExceptT e m a
    tryJust e = hoistEither . maybeToRight e
