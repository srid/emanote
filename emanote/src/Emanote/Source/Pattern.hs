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
  R.IgnoreFile ->
    ignoreFilePattern
  R.AnyExt ->
    "**"

{- | unionmount assigns the first matching pattern's tag, so 'R.AnyExt'
is appended last by construction — new known types add to
'knownTypes' without disturbing the catch-all's trailing position.
'R.IgnoreFile' must precede 'R.AnyExt' so an @.emanoteignore@ file
is tagged as an ignore file rather than as an arbitrary static asset.
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
      , R.IgnoreFile
      ]

{- | Universal ignore patterns applied to every layer. Layer-specific
ignores belong in a @.emanoteignore@ file at the layer root — see
"Emanote.Source.Ignore".

@.emanoteignore@ deliberately does NOT appear here: 'Emanote.Source.Dynamic'
threads its events through the fsnotify pipeline so mid-session edits
can refresh the model. The 'R.IgnoreFile' branch in
@Emanote.Source.Patch.indexesAsStaticFile@ keeps it from leaking onto
the static-file route despite being unfiltered here.
-}
ignorePatterns :: [FilePattern]
ignorePatterns =
  [ -- Contents of dotfile directories (eg @.git/HEAD@, @.vscode/settings.json@).
    -- The trailing @\/**\/*@ requires at least one path component INSIDE the
    -- dotfile directory — without it, the pattern silently swallows root-level
    -- dotfiles like @.emanoteignore@ (since @\*\*@ matches the empty path),
    -- which would defeat the hot-reload pipeline in 'Emanote.Source.Dynamic'.
    "**/.*/**/*"
  , -- Ignore vi/vim/neovim writebackup files (see ":help writebackup")
    "**/*~"
  , -- /Top-level ./-/ directory is reserved by Emanote
    "-/**"
  ]
