-- | Default files from emanote tree.
module Emanote.Source.Default where

import Control.Monad.Logger (MonadLogger)
import qualified Data.Aeson as Aeson
import Emanote.Logging (log)
import Emanote.Source.Util (chainM, parseSData)
import qualified Heist.Extra.TemplateState as T
import qualified Paths_emanote
import System.Directory (withCurrentDirectory)
import System.FilePath ((</>))
import qualified System.FilePattern.Directory as FP

emanoteDefaultTemplates :: (MonadIO m, MonadLogger m) => m T.TemplateState
emanoteDefaultTemplates = do
  defaultFiles <- liftIO Paths_emanote.getDataDir
  log $ "Loading default templates from " <> toText defaultFiles
  liftIO $
    withCurrentDirectory defaultFiles $ do
      files <- FP.getDirectoryFiles "." ["**/*.tpl"]
      populateTemplates <- chainM files $ \fp -> do
        s <- readFileBS fp
        pure $ T.addTemplateFile fp s
      populateTemplates <$> T.newTemplateState

emanoteDefaultIndexData :: (MonadIO m, MonadLogger m) => m Aeson.Value
emanoteDefaultIndexData = do
  defaultFiles <- liftIO Paths_emanote.getDataDir
  let indexYaml = defaultFiles </> "index.yaml"
  log $ "Loading default index.yaml from " <> toText indexYaml
  parseSData =<< readFileBS indexYaml
