module Emanote.Source.Pattern where

import Emanote.Route qualified as R
import Relude
import System.FilePattern (FilePattern)

filePattern :: HasCallStack => R.FileType R.SourceExt -> FilePath
filePattern = \case
  R.LMLType R.Md ->
    R.withExt @_ @('R.LMLType 'R.Md) $
      "**/*"
  R.LMLType R.Org ->
    R.withExt @_ @('R.LMLType 'R.Org) $
      "**/*"
  R.Yaml ->
    R.withExt @_ @'R.Yaml $
      "**/*"
  R.HeistTpl ->
    R.withExt @_ @'R.HeistTpl $
      "**/*"
  R.AnyExt ->
    "**"

filePatterns :: [(R.FileType R.SourceExt, FilePattern)]
filePatterns =
  (id &&& filePattern)
    <$> [ R.LMLType R.Md,
          R.LMLType R.Org,
          R.Yaml,
          R.HeistTpl,
          R.AnyExt
        ]

ignorePatterns :: [FilePattern]
ignorePatterns =
  [ -- Ignore all dotfile directories (eg: .git, .vscode)
    "**/.*/**",
    -- Ignore vi/vim/neovim writebackup files (see ":help writebackup")
    "**/*~",
    -- /Top-level ./-/ directory is reserved by Emanote
    "-/**"
  ]
