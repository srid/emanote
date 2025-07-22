{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Emanote.CLI (
  Cli (..),
  Layer (..),
  Cmd (..),
  ExportCmd (..),
  parseCli,
  cliParser,
) where

import Data.Text qualified as T
import Data.Version (showVersion)
import Ema.CLI qualified
import Options.Applicative hiding (action)
import Paths_emanote qualified
import Relude
import UnliftIO.Directory (getCurrentDirectory)

data Cli = Cli
  { layers :: NonEmpty Layer
  , allowBrokenLinks :: Bool
  , cmd :: Cmd
  }

data Layer = Layer
  { path :: FilePath
  , mountPoint :: Maybe FilePath
  }

data Cmd
  = Cmd_Ema Ema.CLI.Cli
  | Cmd_Export ExportCmd

data ExportCmd
  = ExportCmd_Metadata
  | ExportCmd_Content
      { exportContentDeployedSite :: Maybe Text
      }

cliParser :: FilePath -> Parser Cli
cliParser cwd = do
  layers <- layerList $ one $ Layer cwd Nothing
  allowBrokenLinks <- switch (long "allow-broken-links" <> help "Report but do not fail on broken links")
  cmd <-
    fmap Cmd_Ema Ema.CLI.cliParser
      <|> subparser (command "export" (info exportParser (progDesc "Export commands")))
  pure Cli {..}
  where
    exportParser :: Parser Cmd
    exportParser = do
      exportCmd <-
        subparser 
          ( command "metadata" (info (pure ExportCmd_Metadata) (progDesc "Export metadata JSON"))
            <> command "content" (info contentExportParser (progDesc "Export all content to a single Markdown file"))
          )
      pure $ Cmd_Export exportCmd
    contentExportParser :: Parser ExportCmd
    contentExportParser = do
      deployedSite <- optional $ strOption
        ( long "deployed-site"
          <> metavar "URL"
          <> help "Base URL of the deployed site for source references"
        )
      pure $ ExportCmd_Content deployedSite
    layerList defaultPath = do
      option layerListReader
        $ mconcat
          [ long "layers"
          , short 'L'
          , metavar "LAYERS"
          , value defaultPath
          , help "List of (semicolon delimited) notebook folders to 'union mount', with the left-side folders being overlaid on top of the right-side ones. The default layer is implicitly included at the end of this list."
          ]
    layerListReader :: ReadM (NonEmpty Layer)
    layerListReader = do
      let partition s =
            T.breakOn "@" s
              & second (\x -> if T.null s then Nothing else Just $ T.drop 1 x)
      maybeReader $ \paths ->
        nonEmpty
          $ fmap (uncurry Layer . bimap toString (fmap toString) . partition)
          $ T.split (== ';')
          . toText
          $ paths

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
