{- | Load user-controllable ignore patterns from a per-layer
@.emanoteignore@ file.

Each layer (user or default) may contain a top-level @.emanoteignore@
listing one `FilePattern` per line. Blank lines and lines beginning
with @#@ are skipped. Patterns are scoped to the layer they live in,
so a pattern in layer A's file does not affect files inside layer B.
-}
module Emanote.Source.Ignore (
  loadIgnorePatterns,
  ignoreFileName,
  ignoreFilePattern,
  parsePatterns,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Emanote.Source.Loc (Loc, locPath)
import Relude
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.FilePattern (FilePattern)

{- | The filename Emanote reads ignore patterns from, relative to each
layer's root.
-}
ignoreFileName :: FilePath
ignoreFileName = ".emanoteignore"

{- | The `FilePattern` that matches the ignore file at any depth. Used
by "Emanote.Source.Pattern" to keep the configuration file off the
static-file route.
-}
ignoreFilePattern :: FilePattern
ignoreFilePattern = "**/" <> ignoreFileName

{- | Read @.emanoteignore@ from each layer's root and return a map keyed
by `Loc`. Layers without an ignore file (or with an empty one) are
absent from the result; `unionMount` treats absence as "no per-source
patterns," which is the correct behavior for those layers.
-}
loadIgnorePatterns :: (MonadIO m) => Set Loc -> m (Map Loc [FilePattern])
loadIgnorePatterns layers =
  fmap (Map.fromList . catMaybes) $ forM (Set.toList layers) $ \loc -> do
    let path = fst (locPath loc) </> ignoreFileName
    liftIO (doesFileExist path) >>= \case
      False -> pure Nothing
      True -> do
        contents <- decodeUtf8 <$> readFileBS path
        case parsePatterns contents of
          [] -> pure Nothing
          pats -> pure $ Just (loc, pats)

{- | Strip blanks and comments, then return one pattern per remaining
line. Comment syntax is a literal @#@ at the start of the line — no
mid-line comments and no escape sequences.
-}
parsePatterns :: Text -> [FilePattern]
parsePatterns =
  mapMaybe lineToPattern . lines
  where
    lineToPattern line = do
      let trimmed = T.strip line
      guard $ not (T.null trimmed)
      guard $ not ("#" `T.isPrefixOf` trimmed)
      pure $ toString trimmed
