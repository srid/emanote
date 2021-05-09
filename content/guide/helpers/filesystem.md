---
order: 2
---
# Working with files

If your static site is generated depending on local files on disk, the general flow of things is as follows:

```haskell
runEma render $ \model -> do
  -- Load everything on launch
  initialModel <- loadFilesAndBuildModel
  LVar.set model initialModel
  -- Continue to monitor and update the model
  observeFileSystem $ \action -> 
    LVar.modify model $ applyAction action
```

For monitoring local files on disk you would typically use something like [fsnotify](https://hackage.haskell.org/package/fsnotify) in place of `observeFileSystem`. What is the point of doing this? To support [hot reload](concepts/hot-reload.md) on _data_ change. Imagine that your static site is generated based on Markdown files as well as HTML templates on disk. If either the Markdown file, or a HTML template file is modified, we want the web browser to hot reload the updated HTML *instantly*. This is enabled by storing both these kinds of files in the application [model](guide/model.md) and using [LVar](concepts/lvar.md) to update it *over time*.

For filesystem changes, Ema provides a helper based on `fsnotify` in the `Ema.Helper.FileSystem` module. You can use it as follows

```haskell
import qualified Ema.Helper.FileSystem as FileSystem

type Model = Map FilePath Text

Ema.runEma render $ \model -> do
  LVar.set model =<< do
    mdFiles <- FileSystem.filesMatching "." ["**/*.md"]
    forM mdFiles readFileText
      <&> Map.fromList 
  FileSystem.onChange "." $ \fp -> \case
    FileSystem.Update ->
      when (takeExtension fp == ".md") $ do
        log $ "Update: " <> fp 
        s <- readFileText fp
        LVar.modify model $ Map.insert fp s
    FileSystem.Delete ->
      whenJust (takeExtension fp == ".md") $ do
        log $ "Delete: " <> fp
        LVar.modify model $ Map.delete fp
```

In most cases, however, you probably want to use the higher level function `mountOnLVar`. It "mounts" the files you specify onto the [model LVar](concepts/lvar.md) such that any changes to them are *automatically* reflected in your [model](guide/model.md) value.

```haskell
Ema.runEma render $ \model -> do
  FileSystem.mountOnLVar "." ["**/*.md"] model $ \fp -> \case
    FileSystem.Update -> do
      s <- readFileText fp
      pure $ Map.insert fp s
    FileSystem.Delete ->
      pure $ Map.delete fp
```

[Full example here](https://github.com/srid/ema-docs/blob/master/src/Main.hs).