{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Emanote.Cli where

import qualified Ema.CLI
import Options.Applicative hiding (action)

data Cli = Cli {overrideIndexData :: Maybe Text, emaCli :: Ema.CLI.Cli}

cliParser :: Parser Cli
cliParser = do
  overrideIndexData <-
    optional $
      strOption $
        mconcat
          [ long "override-index-data",
            metavar "JSON",
            help "Override the default parameters in index.yaml. For example, to set the baseUrl, use {\"template\": {\"baseUrl\": URL}}."
          ]
  emaCli <- Ema.CLI.cliParser
  pure Cli {..}

parseCli :: ParserInfo Cli
parseCli =
  info
    (cliParser <**> helper)
    ( fullDesc
        <> progDesc "Emanote - A spiritual successor to Neuron"
        <> header "Emanote"
    )