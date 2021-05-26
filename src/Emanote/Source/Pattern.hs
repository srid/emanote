{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.Source.Pattern where

import qualified Emanote.Route.Ext as Ext
import System.FilePattern (FilePattern)

filePattern :: Ext.FileType -> FilePath
filePattern = \case
  Ext.LMLType Ext.Md ->
    Ext.withExt @('Ext.LMLType 'Ext.Md) $
      "**/*"
  Ext.Yaml ->
    Ext.withExt @'Ext.Yaml $
      "**/*"
  Ext.Html ->
    Ext.withExt @'Ext.Html $
      "**/*"
  Ext.HeistTpl ->
    Ext.withExt @'Ext.HeistTpl $
      "**/*"
  Ext.AnyExt ->
    "**"

filePatterns :: [(Ext.FileType, FilePattern)]
filePatterns =
  (id &&& filePattern)
    <$> [ Ext.LMLType Ext.Md,
          Ext.Yaml,
          Ext.HeistTpl,
          Ext.AnyExt
        ]

ignorePatterns :: [FilePattern]
ignorePatterns =
  [ -- Ignore all top-level dotfile directories (eg: .git, .vscode)
    ".*/**"
  ]
