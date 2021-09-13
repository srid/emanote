-- TODO: Eventually move `Heist.Extra` to its own library
-- Consider first the full practical range of template patterns,
-- https://softwaresimply.blogspot.com/2011/04/looping-and-control-flow-in-heist.html
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Heist.Extra.TemplateState
  ( TemplateState,
    newTemplateState,
    addTemplateFile,
    removeTemplateFile,
    renderHeistTemplate,
  )
where

import Control.Lens.Operators ((.~))
import Control.Monad.Except (runExcept)
import Data.ByteString.Builder (toLazyByteString)
import Data.Default (Default (..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Heist as H
import qualified Heist.Common as H
import qualified Heist.Extra as HE
import qualified Heist.Internal.Types as HT
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
      "Heist templates loaded: " <> toString (T.intercalate ", " $ HE.availableTemplates st)

newTemplateState :: MonadIO m => m TemplateState
newTemplateState = do
  let heistCfg :: H.HeistConfig Identity =
        H.emptyHeistConfig
          & H.hcNamespace .~ ""
  liftIO $ TemplateState <$> H.initHeist heistCfg

addTemplateFile :: HasCallStack => FilePath -> FilePath -> ByteString -> TemplateState -> TemplateState
addTemplateFile fp fpRel s (TemplateState eSt) =
  TemplateState $ do
    st <- eSt
    first one (XmlHtml.parseHTML fp s) >>= \case
      XmlHtml.XmlDocument {} ->
        -- TODO: Need this for RSS feed generation.
        Left $ one "Xml unsupported"
      XmlHtml.HtmlDocument {..} -> do
        Right $ HI.addTemplate (tmplName fpRel) docContent (Just fp) st

tmplName :: HasCallStack => String -> ByteString
tmplName fp = fromMaybe (error "Not a .tpl file") $ do
  let (base, ext) = splitExtension fp
  guard $ ext == ".tpl"
  pure $ encodeUtf8 base

removeTemplateFile :: HasCallStack => FilePath -> TemplateState -> TemplateState
removeTemplateFile fp (TemplateState eSt) =
  TemplateState $ do
    st <- eSt
    let tpath = H.splitPathWith '/' (tmplName fp)
    Right $ st {HT._templateMap = HM.delete tpath (HT._templateMap st)}

renderHeistTemplate ::
  HasCallStack =>
  Text ->
  H.Splices (HI.Splice Identity) ->
  TemplateState ->
  Either Text LByteString
renderHeistTemplate name splices st =
  runExcept $ do
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
