{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Emanote.CLI
  ( Cli (..),
    parseCli,
  )
where

import Data.Text qualified as T
import Data.Version (showVersion)
import Ema.CLI qualified
import Options.Applicative hiding (action)
import Paths_emanote qualified
import Relude
import UnliftIO.Directory (getCurrentDirectory)

data Cli = Cli
  { layers :: NonEmpty FilePath,
    test :: Bool,
    allowBrokenLinks :: Bool,
    emaCli :: Ema.CLI.Cli
  }

cliParser :: FilePath -> Parser Cli
cliParser cwd = do
  layers <- pathList (one cwd)
  test <- switch (long "test" <> help "Run tests")
  allowBrokenLinks <- switch (long "allow-broken-links" <> help "Report but do not fail on broken links")
  emaCli <- Ema.CLI.cliParser
  pure Cli {..}
  where
    pathList defaultPath = do
      option pathListReader $
        mconcat
          [ long "layers",
            short 'L',
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
