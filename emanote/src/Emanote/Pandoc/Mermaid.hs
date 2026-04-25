{-# LANGUAGE LambdaCase #-}

{- | Render `mermaid` code blocks to inline SVG at build time.

The site is fully renderable offline once SVG is baked into the HTML, with
no client-side JavaScript or CDN dependency for diagrams. Falls back to
preserving the original code block (with an HTML comment carrying the
error) when @mmdc@ is missing or rendering fails.
-}
module Emanote.Pandoc.Mermaid (
  transformMermaidBlocks,
  -- For testing
  hasMermaidBlock,
  stripXmlPrologue,
) where

import Control.Monad.Logger (MonadLogger)
import Data.Text qualified as T
import Emanote.Prelude (logE, logW)
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

mmdcExe :: String
mmdcExe = "mmdc"

{- | Walk the Pandoc AST and replace mermaid code blocks with inline SVG.

If @mmdc@ is unavailable on PATH, the document is returned unchanged after
a single warning log line. Per-diagram failures preserve the original
block alongside an HTML comment with the underlying error.
-}
transformMermaidBlocks :: (MonadIO m, MonadLogger m) => B.Pandoc -> m B.Pandoc
transformMermaidBlocks doc
  | not (hasMermaidBlock doc) = pure doc
  | otherwise =
      liftIO (findExecutable mmdcExe) >>= \case
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

renderBlock :: (MonadIO m, MonadLogger m) => FilePath -> B.Block -> m B.Block
renderBlock bin blk = case blk of
  B.CodeBlock (_, classes, _) code
    | mermaidClass `elem` classes ->
        runMmdc bin code >>= \case
          Right svg -> pure $ B.RawBlock (B.Format "html") svg
          Left err -> do
            logE $ "mermaid render failed: " <> err
            pure $ B.Div ("", ["mermaid-error"], []) [errorComment err, blk]
  _ -> pure blk

errorComment :: Text -> B.Block
errorComment err =
  B.RawBlock (B.Format "html")
    $ "<!-- mermaid render failed: "
    <> T.replace "-->" "--&gt;" err
    <> " -->"

runMmdc :: (MonadIO m) => FilePath -> Text -> m (Either Text Text)
runMmdc bin code = liftIO $ withSystemTempDirectory "emanote-mermaid" $ \tmpDir -> do
  let inputPath = tmpDir </> "diagram.mmd"
      outputPath = tmpDir </> "diagram.svg"
  writeFileBS inputPath (encodeUtf8 code)
  (exitCode, _stdout, stderr) <-
    readProcessWithExitCode bin ["-i", inputPath, "-o", outputPath] ""
  case exitCode of
    ExitSuccess ->
      doesFileExist outputPath >>= \case
        True -> Right . stripXmlPrologue . decodeUtf8 <$> readFileBS outputPath
        False -> pure $ Left "mmdc exited 0 but produced no SVG file"
    ExitFailure n ->
      pure
        . Left
        $ "mmdc exited "
        <> show n
        <> ": "
        <> T.strip (toText stderr)

{- | Drop @<?xml ...?>@ and @<!DOCTYPE ...>@ prefixes so the SVG can be
embedded directly into HTML.
-}
stripXmlPrologue :: Text -> Text
stripXmlPrologue = go . T.stripStart
  where
    go t
      | "<?xml" `T.isPrefixOf` t = go . T.stripStart . T.drop 1 . T.dropWhile (/= '>') $ t
      | "<!DOCTYPE" `T.isPrefixOf` t = go . T.stripStart . T.drop 1 . T.dropWhile (/= '>') $ t
      | otherwise = t
