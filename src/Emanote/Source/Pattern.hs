{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.Source.Pattern where

import qualified Emanote.Route as R
import Relude
import System.FilePattern (FilePattern)

filePattern :: HasCallStack => R.FileType -> FilePath
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
  R.Folder ->
    error "Unsupported"

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
  [ -- Ignore all dotfile directories (eg: .git, .vscode)
    "**/.*/**",
    -- /Top-levels beginning with '@' are reserved by Emanote
    "@*/**"
  ]
