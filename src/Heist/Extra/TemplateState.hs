{-# LANGUAGE RecordWildCards #-}

-- TODO: Eventually move `Heist.Extra` to its own library
-- Consider first the full practical range of template patterns,
-- https://softwaresimply.blogspot.com/2011/04/looping-and-control-flow-in-heist.html

module Heist.Extra.TemplateState
  ( TemplateState,
    TemplateName,
    addTemplateFile,
    removeTemplateFile,
    renderHeistTemplate,
  )
where

import Control.Monad.Except (MonadError (throwError), runExcept)
import Data.ByteString.Builder (toLazyByteString)
import Data.Default (Default (..))
import Data.HashMap.Strict qualified as HM
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import GHC.IO.Unsafe (unsafePerformIO)
import Heist qualified as H
import Heist.Common qualified as H
import Heist.Internal.Types qualified as HT
import Heist.Interpreted qualified as HI
import Optics.Operators ((.~))
import Relude
import System.FilePath (splitExtension)
import Text.XmlHtml qualified as XmlHtml

type TemplateName = ByteString

data TemplateState = TemplateState (H.HeistState Identity) TemplateErrors

instance Default TemplateState where
  def = unsafePerformIO emptyTemplateState

emptyTemplateState :: MonadIO m => m TemplateState
emptyTemplateState = do
  let heistCfg :: H.HeistConfig Identity =
        H.emptyHeistConfig
          & H.hcNamespace (const $ Identity "")
          & runIdentity
  eSt <- liftIO $ H.initHeist heistCfg
  let st = either (error . T.intercalate "," . fmap toText) id eSt
  pure $ TemplateState st mempty

getTemplateState :: MonadError Text m => TemplateState -> m (HT.HeistState Identity)
getTemplateState (TemplateState st errs) =
  if not $ null errs
    then throwError $ showErrors errs
    else pure st

addTemplate :: TemplateName -> FilePath -> TemplateState -> Either Text HT.Template -> TemplateState
addTemplate name fp (TemplateState st errs) = \case
  Left err ->
    TemplateState st (assignError name err errs)
  Right doc ->
    let newSt = HI.addTemplate name doc (Just fp) st
     in TemplateState newSt (clearError name errs)

addTemplateFile ::
  HasCallStack =>
  -- | Absolute path
  FilePath ->
  -- | Relative path (to template base)
  FilePath ->
  -- | Contents of the .tmpl file
  ByteString ->
  TemplateState ->
  TemplateState
addTemplateFile fp (tmplName -> name) s tmplSt =
  addTemplate name fp tmplSt $
    XmlHtml.parseHTML fp s & \case
      Left (toText -> err) ->
        Left err
      Right XmlHtml.XmlDocument {} ->
        Left "Xml unsupported"
      Right XmlHtml.HtmlDocument {..} ->
        Right docContent

removeTemplate :: HasCallStack => TemplateName -> TemplateState -> TemplateState
removeTemplate name (TemplateState st errs) =
  let tpath = H.splitPathWith '/' name
      newSt = st {HT._templateMap = HM.delete tpath (HT._templateMap st)}
   in TemplateState newSt (clearError name errs)

removeTemplateFile :: HasCallStack => FilePath -> TemplateState -> TemplateState
removeTemplateFile (tmplName -> name) = removeTemplate name

tmplName :: HasCallStack => String -> TemplateName
tmplName fp = fromMaybe (error "Not a .tpl file") $ do
  let (base, ext) = splitExtension fp
  guard $ ext == ".tpl"
  pure $ encodeUtf8 base

renderHeistTemplate ::
  HasCallStack =>
  TemplateName ->
  H.Splices (HI.Splice Identity) ->
  TemplateState ->
  Either Text LByteString
renderHeistTemplate name splices tmplSt =
  runExcept $ do
    st <- getTemplateState tmplSt
    (builder, _mimeType) <-
      tryJust ("Unable to render template '" <> decodeUtf8 name <> "'") $
        runIdentity $
          HI.renderTemplate (HI.bindSplices splices st) name
    pure $ toLazyByteString builder
  where
    -- A 'fromJust' that fails in the 'ExceptT' monad
    tryJust :: Monad m => e -> Maybe a -> ExceptT e m a
    tryJust e = hoistEither . maybeToRight e

-- | Type to track errors on a per template basis
type TemplateErrors = Map TemplateName Text

showErrors :: TemplateErrors -> Text
showErrors m =
  T.intercalate "\n" $
    Map.toList m <&> \(k, v) ->
      decodeUtf8 k <> ":\n" <> T.unlines (indent <$> T.lines v)
  where
    indent :: Text -> Text
    indent s = "\t" <> s

-- | Assign a new error for the given template
assignError :: TemplateName -> Text -> TemplateErrors -> TemplateErrors
assignError =
  Map.insert

-- | Indicate that the given template has no errors
clearError :: TemplateName -> TemplateErrors -> TemplateErrors
clearError =
  Map.delete
