{- | Load user-controllable ignore patterns from a per-layer
@.emanoteignore@ file, and re-read them mid-session when those files
change.

Each layer (user or default) may contain a top-level @.emanoteignore@
listing one `FilePattern` per line. Blank lines and lines beginning
with @#@ are skipped. Patterns are scoped to the layer they live in,
so a pattern in layer A's file does not affect files inside layer B.

The static loader 'loadIgnorePatterns' is the startup path. The
mid-session hot-reload path lives in 'Emanote.Source.Dynamic' and
calls 'readIgnoreFile' on each fsnotify event for an 'R.IgnoreFile';
the parsed patterns are stored in a 'TVar' so unionmount's overlay
filter sees the new values immediately and the model walk that
follows knows which notes flipped sides.
-}
module Emanote.Source.Ignore (
  -- * Loading patterns
  loadIgnorePatterns,
  readIgnoreFile,
  parsePatterns,

  -- * Filenames and patterns
  ignoreFileName,
  ignoreFilePattern,
  isLayerRootIgnoreFile,

  -- * Applying patterns
  matchesAnyPattern,
  isLayerPathIgnored,
  OverlayOutcome (..),
  classifyOverlays,
) where

import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Emanote.Source.Loc (Loc, locPath)
import Relude
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.FilePattern (FilePattern, (?==))

{- | The filename Emanote reads ignore patterns from, relative to each
layer's root.
-}
ignoreFileName :: FilePath
ignoreFileName = ".emanoteignore"

{- | The `FilePattern` that matches the ignore file at any depth.
Used in "Emanote.Source.Pattern" to surface ignore-file events
through unionmount as their own 'R.IgnoreFile' tag.
-}
ignoreFilePattern :: FilePattern
ignoreFilePattern = "**/" <> ignoreFileName

{- | Read @.emanoteignore@ from each layer's root and return a map keyed
by `Loc`. Layers without an ignore file (or with an empty one) are
absent from the result.
-}
loadIgnorePatterns :: (MonadIO m) => Set Loc -> m (Map Loc [FilePattern])
loadIgnorePatterns layers =
  fmap (Map.fromList . catMaybes) $ forM (Set.toList layers) $ \loc -> do
    readIgnoreFile (fst (locPath loc) </> ignoreFileName) <&> \case
      [] -> Nothing
      pats -> Just (loc, pats)

{- | Read and parse a single @.emanoteignore@ file. Returns @[]@ when
the file is absent, empty, or parses to no usable patterns — the
"no patterns" semantic is the same in all three cases, which keeps
callers from re-tripping over 'doesFileExist'.
-}
readIgnoreFile :: (MonadIO m) => FilePath -> m [FilePattern]
readIgnoreFile path =
  liftIO (doesFileExist path) >>= \case
    False -> pure []
    True -> parsePatterns . decodeUtf8 <$> readFileBS path

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

{- | Is the given layer-relative path the @.emanoteignore@ file at the
layer root? Sub-tree ignore files (e.g. @sub/.emanoteignore@) are
delivered to the handler by unionmount under the same 'R.IgnoreFile'
tag but are configuration noise — only the layer-root file is read.
-}
isLayerRootIgnoreFile :: FilePath -> Bool
isLayerRootIgnoreFile = (== ignoreFileName)

-- | Does any of the patterns match the given layer-relative path?
matchesAnyPattern :: [FilePattern] -> FilePath -> Bool
matchesAnyPattern pats fp = any (?== fp) pats

{- | Is a given layer-relative path ignored under the per-layer
pattern set? Layers absent from the map carry no per-layer patterns.
-}
isLayerPathIgnored :: Map Loc [FilePattern] -> Loc -> FilePath -> Bool
isLayerPathIgnored patterns loc =
  matchesAnyPattern (Map.findWithDefault [] loc patterns)

{- | How a 'NonEmpty (Loc, FilePath)' overlay list classifies under a
pattern set: all entries kept, some kept, or none kept. Captures the
pure decision the streaming handler needs so the side-effecting code
in 'Emanote.Source.Dynamic' can pattern-match without re-deriving it.
-}
data OverlayOutcome
  = -- | No overlay entry matched any pattern.
    OverlayKept
  | -- | At least one entry survived, but at least one was filtered.
    OverlayPartial (NonEmpty (Loc, FilePath))
  | -- | Every entry matched a pattern.
    OverlayDropped
  deriving stock (Eq, Show)

classifyOverlays :: Map Loc [FilePattern] -> NonEmpty (Loc, FilePath) -> OverlayOutcome
classifyOverlays patterns overlays
  -- Fast path: most notebooks have no per-layer patterns at all,
  -- and even those that do leave most fsnotify events unaffected.
  -- Skip the @NE.filter@ + per-overlay 'Map.findWithDefault' walk.
  | Map.null patterns = OverlayKept
  | otherwise =
      let survivors = NE.filter (\(loc, lfp) -> not (isLayerPathIgnored patterns loc lfp)) overlays
       in case nonEmpty survivors of
            Nothing -> OverlayDropped
            Just survNE
              | length survivors == NE.length overlays -> OverlayKept
              | otherwise -> OverlayPartial survNE
