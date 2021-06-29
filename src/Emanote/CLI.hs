{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Emanote.CLI
  ( Cli (..),
    parseCli,
  )
where

import qualified Data.Text as T
import Data.Version (showVersion)
import qualified Ema.CLI
import Options.Applicative hiding (action)
import qualified Paths_emanote
import UnliftIO.Directory (getCurrentDirectory)

data Cli = Cli
  { layers :: NonEmpty FilePath,
    emaCli :: Ema.CLI.Cli
  }

cliParser :: FilePath -> Parser Cli
cliParser cwd = do
  layers <- pathList (one cwd)
  emaCli <- Ema.CLI.cliParser
  pure Cli {..}
  where
    pathList defaultPath = do
      option pathListReader $
        mconcat
          [ long "layers",
            metavar "LAYERS",
            value defaultPath,
            help "List of notebook folders to 'union mount', with the right-side folders taking priority."
          ]
    pathListReader :: ReadM (NonEmpty FilePath)
    pathListReader =
      maybeReader $ \paths ->
        nonEmpty $ fmap toString $ T.split (== ';') . toText $ paths

parseCli' :: FilePath -> ParserInfo Cli
parseCli' cwd =
  info
    (versionOption <*> cliParser cwd <**> helper)
    ( fullDesc
        <> progDesc "Emanote - A spiritual successor to Neuron"
        <> header "Emanote"
    )
  where
    versionOption =
      infoOption
        (showVersion Paths_emanote.version)
        (long "version" <> help "Show version")

parseCli :: IO Cli
parseCli =
  execParser . parseCli' =<< getCurrentDirectory
