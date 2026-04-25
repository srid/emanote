{-# LANGUAGE LambdaCase #-}

{- | Render `mermaid` code blocks to inline SVG at build time.

The site is fully renderable offline once SVG is baked into the HTML, with
no client-side JavaScript or CDN dependency for diagrams. Falls back to
preserving the original code block alongside a visible error message when
@mmdc@ is missing or rendering fails.
-}
module Emanote.Pandoc.Mermaid (
  transformMermaidBlocks,
  -- For testing
  hasMermaidBlock,
  stripXmlPrologue,
) where

import Control.Monad.Logger (MonadLogger)
import Control.Monad.Writer.Strict (MonadWriter (tell))
import Data.Text qualified as T
import Emanote.Prelude (logW)
import Relude
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Text.Pandoc.Definition qualified as B
import Text.Pandoc.Walk qualified as W
import UnliftIO.Directory (doesFileExist)
import UnliftIO.Process (readProcessWithExitCode)
import UnliftIO.Temporary (withSystemTempDirectory)

mermaidClass :: Text
mermaidClass = "mermaid"

{- | Walk the Pandoc AST and replace mermaid code blocks with inline SVG.

If @mmdc@ is unavailable on PATH, the document is returned unchanged after
a single warning log line. Per-diagram render failures are reported via
@tell@ so they surface in the same per-note error block as parse failures.
-}
transformMermaidBlocks :: (MonadIO m, MonadLogger m, MonadWriter [Text] m) => B.Pandoc -> m B.Pandoc
transformMermaidBlocks doc
  | not (hasMermaidBlock doc) = pure doc
  | otherwise =
      liftIO (findExecutable "mmdc") >>= \case
        Nothing -> do
          logW
            $ "mmdc not found on PATH; mermaid code blocks will not be rendered to "
            <> "inline SVG. Install @mermaid-js/mermaid-cli to enable offline "
            <> "diagram rendering."
          pure doc
        Just bin -> W.walkM (renderBlock bin) doc

hasMermaidBlock :: B.Pandoc -> Bool
hasMermaidBlock = getAny . W.query check
  where
    check (B.CodeBlock (_, classes, _) _) = Any (mermaidClass `elem` classes)
    check _ = mempty

renderBlock :: (MonadIO m, MonadWriter [Text] m) => FilePath -> B.Block -> m B.Block
renderBlock bin blk = case blk of
  B.CodeBlock (_, classes, _) code
    | mermaidClass `elem` classes ->
        runMmdc bin code >>= \case
          Right svg -> pure $ B.RawBlock (B.Format "html") svg
          Left err -> do
            tell ["mermaid render failed: " <> err]
            pure $ B.Div ("", ["mermaid-error"], []) [errorMessage err, blk]
  _ -> pure blk

errorMessage :: Text -> B.Block
errorMessage err =
  B.Para
    [ B.Strong [B.Str "Mermaid rendering failed:"]
    , B.Space
    , B.Code mempty err
    ]

runMmdc :: (MonadIO m) => FilePath -> Text -> m (Either Text Text)
runMmdc bin code = liftIO $ withSystemTempDirectory "emanote-mermaid" $ \tmpDir -> do
  let inputPath = tmpDir </> "diagram.mmd"
      outputPath = tmpDir </> "diagram.svg"
  writeFileBS inputPath (encodeUtf8 code)
  (exitCode, _stdout, stderr) <-
    readProcessWithExitCode bin ["-i", inputPath, "-o", outputPath] ""
  case exitCode of
    ExitFailure n ->
      pure . Left $ "mmdc exited " <> show n <> ": " <> T.strip (toText stderr)
    ExitSuccess -> do
      exists <- doesFileExist outputPath
      if exists
        then Right . stripXmlPrologue . decodeUtf8 <$> readFileBS outputPath
        else pure $ Left "mmdc exited 0 but produced no SVG file"

{- | Strip leading XML processing instructions (@\<?...?\>@) and tag-style
declarations (@\<!DOCTYPE ...\>@) so the SVG can be embedded directly into
HTML. Unrecognised or malformed prefixes are left in place rather than
silently truncated.
-}
stripXmlPrologue :: Text -> Text
stripXmlPrologue = go . T.stripStart
  where
    go t = case stripOnePrologue t of
      Just rest -> go (T.stripStart rest)
      Nothing -> t
    stripOnePrologue t
      | Just rest <- T.stripPrefix "<?" t = stripUntil "?>" rest
      | Just rest <- T.stripPrefix "<!DOCTYPE" t = stripUntil ">" rest
      | otherwise = Nothing
    stripUntil terminator t = case T.breakOn terminator t of
      (_, after) | T.null after -> Nothing
      (_, after) -> Just (T.drop (T.length terminator) after)
