{-# LANGUAGE TypeApplications #-}

module Heist.Extra where

import qualified Data.Text as T
import qualified Heist as H
import qualified Heist.Common as H
import qualified Heist.Internal.Types as HT
import qualified Heist.Interpreted as HI
import qualified Text.XmlHtml as X

-- | Useful for running a splice against an arbitrary node (such as that pulled from pandoc.tpl)
runCustomNode :: Monad n => X.Node -> H.Splices (HI.Splice n) -> HI.Splice n
runCustomNode node splices =
  H.localHS (HI.bindSplices splices) $ do
    HI.runNode node <&> \case
      [resNode]
        | X.elementTag resNode == X.elementTag node ->
          -- Get rid of the `node` itself.
          X.elementChildren resNode
      res ->
        res

runCustomTemplate :: Monad n => HT.Template -> H.Splices (HI.Splice n) -> HI.Splice n
runCustomTemplate nodes splices =
  H.localHS (HI.bindSplices splices) $ do
    HI.runNodeList nodes

lookupHtmlTemplate :: Monad n => ByteString -> HT.HeistT m n (Maybe HT.Template)
lookupHtmlTemplate name = do
  st <- HT.getHS
  pure $ do
    X.HtmlDocument _ _ nodes <- H.dfDoc . fst <$> H.lookupTemplate name st HT._templateMap
    pure nodes

lookupHtmlTemplateMust :: forall m n. Monad n => ByteString -> HT.HeistT m n HT.Template
lookupHtmlTemplateMust name =
  lookupHtmlTemplate name >>= \case
    Nothing -> do
      st <- HT.getHS
      error $ "heist: " <> decodeUtf8 name <> " not found ... among: " <> T.intercalate ", " (availableTemplates st)
    Just tpl ->
      pure tpl

availableTemplates :: HT.HeistState n -> [Text]
availableTemplates st =
  sort $ H.templateNames st <&> T.intercalate "/" . reverse . fmap (decodeUtf8 @Text)