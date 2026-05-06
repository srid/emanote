module Emanote.Source.Pattern where

import Emanote.Route qualified as R
import Emanote.Source.Ignore (ignoreFilePattern)
import Relude
import System.FilePattern (FilePattern)

filePattern :: (HasCallStack) => R.FileType R.SourceExt -> FilePath
filePattern = \case
  R.LMLType R.Md ->
    R.withExt @_ @('R.LMLType 'R.Md)
      $ "**/*"
  R.LMLType R.Org ->
    R.withExt @_ @('R.LMLType 'R.Org)
      $ "**/*"
  R.Yaml ->
    R.withExt @_ @'R.Yaml "**/*"
  R.HeistTpl ->
    R.withExt @_ @'R.HeistTpl
      $ "**/*"
  R.LuaFilter ->
    R.withExt @_ @'R.LuaFilter
      $ "**/*"
  R.AnyExt ->
    "**"

{- | Order matters: 'unionmount' assigns a file to the first pattern it
matches (see @getTag@ / @listToMaybe@ in @System.UnionMount@). 'LuaFilter'
must precede 'AnyExt' so that @.lua@ files are tracked as filters rather
than copied into @_site/@ as static assets.
-}
filePatterns :: [(R.FileType R.SourceExt, FilePattern)]
filePatterns =
  (id &&& filePattern)
    <$> [ R.LMLType R.Md
        , R.LMLType R.Org
        , R.Yaml
        , R.HeistTpl
        , R.LuaFilter
        , R.AnyExt
        ]

{- | Universal ignore patterns applied to every layer. Layer-specific
ignores belong in a `.emanoteignore` file at the layer root — see
"Emanote.Source.Ignore".
-}
ignorePatterns :: [FilePattern]
ignorePatterns =
  [ -- Ignore all dotfile directories (eg: .git, .vscode)
    "**/.*/**"
  , -- Ignore vi/vim/neovim writebackup files (see ":help writebackup")
    "**/*~"
  , -- /Top-level ./-/ directory is reserved by Emanote
    "-/**"
  , -- The ignore file itself is configuration, not a note or static asset.
    ignoreFilePattern
  ]
