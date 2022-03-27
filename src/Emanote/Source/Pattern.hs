module Emanote.Source.Pattern where

import Emanote.Route qualified as R
import Relude
import System.FilePattern (FilePattern)

filePattern :: HasCallStack => R.FileType R.SourceExt -> FilePath
filePattern = \case
  R.LMLType R.Md ->
    R.withExt @R.SourceExt @( 'R.LMLType 'R.Md) $
      "**/*"
  R.Yaml ->
    R.withExt @R.SourceExt @ 'R.Yaml $
      "**/*"
  R.HeistTpl ->
    R.withExt @R.SourceExt @ 'R.HeistTpl $
      "**/*"
  R.AnyExt ->
    "**"

filePatterns :: [(R.FileType R.SourceExt, FilePattern)]
filePatterns =
  (id &&& filePattern)
    <$> [ R.LMLType R.Md
        , R.Yaml
        , R.HeistTpl
        , R.AnyExt
        ]

ignorePatterns :: [FilePattern]
ignorePatterns =
  [ -- Ignore all dotfile directories (eg: .git, .vscode)
    "**/.*/**"
  , -- /Top-levels beginning with '@' are reserved by Emanote
    "@*/**"
  ]
