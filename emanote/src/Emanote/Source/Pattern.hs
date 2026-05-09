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
    R.withExt @_ @'R.LuaFilter "**/*"
  R.AnyExt ->
    "**"

{- | The set of file types unionmount tags. 'R.AnyExt' is appended
last *structurally* — adding a new known type to 'knownTypes' is
automatically claimed before falling through to the catch-all,
without anyone needing to remember a comment-enforced "AnyExt last"
ordering rule.
-}
filePatterns :: [(R.FileType R.SourceExt, FilePattern)]
filePatterns =
  ((id &&& filePattern) <$> knownTypes)
    <> [(R.AnyExt, filePattern R.AnyExt)]
  where
    knownTypes :: [R.FileType R.SourceExt]
    knownTypes =
      [ R.LMLType R.Md
      , R.LMLType R.Org
      , R.Yaml
      , R.HeistTpl
      , R.LuaFilter
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
