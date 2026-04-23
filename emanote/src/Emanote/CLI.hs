{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Emanote.CLI (
  Cli (..),
  Layer (..),
  Cmd (..),
  RunCmd (..),
  parseCli,
  cliParser,
) where

import Data.Default (def)
import Data.Text qualified as T
import Data.Version (showVersion)
import Ema.CLI qualified
import Emanote.Route.SiteRoute.Type (ExportFormat (..))
import Options.Applicative hiding (action)
import Paths_emanote qualified
import Relude
import UnliftIO.Directory (getCurrentDirectory)

data Cli = Cli
  { layers :: NonEmpty Layer
  , allowBrokenInternalLinks :: Bool
  , verbose :: Bool
  , cmd :: Cmd
  }

data Layer = Layer
  { path :: FilePath
  , mountPoint :: Maybe FilePath
  }

data Cmd
  = Cmd_Run RunCmd
  | Cmd_Gen FilePath
  | Cmd_Export ExportFormat

-- | Arguments for the 'run' subcommand: Ema's live-server args plus Emanote's additions.
data RunCmd = RunCmd
  { runEmaArgs :: Ema.CLI.RunArgs
  , runMcpPort :: Maybe Int
  }

cmdParser :: Parser Cmd
cmdParser =
  hsubparser
    ( mconcat
        [ command "run" (info (Cmd_Run <$> runCmdParser) (progDesc "Run the live server"))
        , command "gen" (info (Cmd_Gen <$> argument str (metavar "DEST")) (progDesc "Generate the static site"))
        , command "export" (info (Cmd_Export <$> exportParser) (progDesc "Export commands"))
        ]
    )
    <|> pure (Cmd_Run defaultRunCmd)
  where
    defaultRunCmd = RunCmd def Nothing

runCmdParser :: Parser RunCmd
runCmdParser =
  RunCmd
    <$> Ema.CLI.runArgsParser
    <*> optional
      ( option portReader
          $ mconcat
            [ long "mcp-port"
            , metavar "PORT"
            , help "Expose an MCP (Model Context Protocol) server on this port alongside the live server"
            ]
      )

exportParser :: Parser ExportFormat
exportParser =
  subparser
    ( command "metadata" (info (pure ExportFormat_Metadata) (progDesc "Export metadata JSON"))
        <> command "content" (info (pure ExportFormat_Content) (progDesc "Export all notes to single Markdown file to stdout (uses baseUrl from notebook config)"))
    )

portReader :: ReadM Int
portReader = eitherReader $ \s ->
  case readMaybe s of
    Just n | n >= 1 && n <= 65535 -> Right n
    _ -> Left $ "Port must be an integer in [1, 65535], got: " <> s

cliParser :: FilePath -> Parser Cli
cliParser cwd = do
  layers <- layerList $ one $ Layer cwd Nothing
  allowBrokenInternalLinks <- switch (long "allow-broken-internal-links" <> help "Report but do not fail on broken internal links")
  verbose <- switch (long "verbose" <> short 'v' <> help "Enable verbose logging")
  cmd <- cmdParser
  pure Cli {..}
  where
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
