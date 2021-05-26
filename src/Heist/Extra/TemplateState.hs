-- TODO: Eventually move `Heist.Extra` to Ema.Helper
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
import qualified Data.ByteString.Char8 as BC
import Data.Default (Default (..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Heist as H
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
      let names :: [Text] = sort $ H.templateNames st <&> T.intercalate "/" . reverse . fmap (decodeUtf8 @Text)
       in "Heist templates: " <> toString (T.intercalate ", " names)

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

tmplName :: String -> ByteString
tmplName fp = fromMaybe (error "Not a .tpl file") $ do
  let (base, ext) = splitExtension fp
  guard $ ext == ".tpl"
  pure $ encodeUtf8 base

removeTemplateFile :: HasCallStack => FilePath -> TemplateState -> TemplateState
removeTemplateFile fp (TemplateState eSt) =
  TemplateState $ do
    st <- eSt
    let tpath = splitPathWith '/' (tmplName fp)
    Right $ st {HT._templateMap = HM.delete tpath (HT._templateMap st)}

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

-- Uhh, a fork of heist function not exposed.

-- | Converts a path into an array of the elements in reverse order.  If the
-- path is absolute, we need to remove the leading slash so the split doesn't
-- leave @\"\"@ as the last element of the TPath.
--
-- FIXME @\"..\"@ currently doesn't work in paths, the solution is non-trivial
splitPathWith :: Char -> ByteString -> H.TPath
splitPathWith s p = if BC.null p then [] else (reverse $ BC.split s path)
  where
    path = if BC.head p == s then BC.tail p else p
