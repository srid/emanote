{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.Source.Pattern where

import qualified Emanote.Route as R
import System.FilePattern (FilePattern)

filePattern :: R.FileType -> FilePath
filePattern = \case
  R.LMLType R.Md ->
    R.withExt @('R.LMLType 'R.Md) $
      "**/*"
  R.Yaml ->
    R.withExt @'R.Yaml $
      "**/*"
  R.Html ->
    R.withExt @'R.Html $
      "**/*"
  R.HeistTpl ->
    R.withExt @'R.HeistTpl $
      "**/*"
  R.AnyExt ->
    "**"

filePatterns :: [(R.FileType, FilePattern)]
filePatterns =
  (id &&& filePattern)
    <$> [ R.LMLType R.Md,
          R.Yaml,
          R.HeistTpl,
          R.AnyExt
        ]

ignorePatterns :: [FilePattern]
ignorePatterns =
  [ -- Ignore all top-level dotfile directories (eg: .git, .vscode)
    ".*/**"
  ]
