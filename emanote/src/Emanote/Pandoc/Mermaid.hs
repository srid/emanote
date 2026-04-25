{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Render `mermaid` code blocks to inline SVG at build time.

The site is fully renderable offline once SVG is baked into the HTML, with
no client-side JavaScript or CDN dependency for diagrams. The path to
@mmdc@ (mermaid-cli) is baked in at compile time via 'staticWhich' — same
pattern as the @stork@ and @tailwindcss@ binaries — so the dep is enforced
by the Nix flake rather than discovered at runtime. Per-diagram render
failures preserve the original code block alongside a visible error message.
-}
module Emanote.Pandoc.Mermaid (
  transformMermaidBlocks,

  -- * Internal helpers (exported for unit tests)
  stripXmlPrologue,
) where

import Control.Monad.Writer.Strict (MonadWriter (tell))
import Data.Text qualified as T
import Relude
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Which (staticWhich)
import Text.Pandoc.Definition qualified as B
import Text.Pandoc.Walk qualified as W
import UnliftIO.Directory (doesFileExist)
import UnliftIO.Process (readProcessWithExitCode)
import UnliftIO.Temporary (withSystemTempDirectory)

mermaidClass :: Text
mermaidClass = "mermaid"

mmdcBin :: FilePath
mmdcBin = $(staticWhich "mmdc")

{- | Walk the Pandoc AST and replace mermaid code blocks with inline SVG.

Per-diagram render failures are reported via @tell@ so they surface in the
same per-note error block as parse failures, and the failing block is left
in place beneath a visible @Mermaid rendering failed:@ message.
-}
transformMermaidBlocks :: (MonadIO m, MonadWriter [Text] m) => B.Pandoc -> m B.Pandoc
transformMermaidBlocks doc
  | hasMermaidBlock doc = W.walkM renderBlock doc
  | otherwise = pure doc

hasMermaidBlock :: B.Pandoc -> Bool
hasMermaidBlock = getAny . W.query check
  where
    check (B.CodeBlock (_, classes, _) _) = Any (mermaidClass `elem` classes)
    check _ = mempty

renderBlock :: (MonadIO m, MonadWriter [Text] m) => B.Block -> m B.Block
renderBlock blk = case blk of
  B.CodeBlock (_, classes, _) code
    | mermaidClass `elem` classes ->
        runMmdc code >>= \case
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

runMmdc :: (MonadIO m) => Text -> m (Either Text Text)
runMmdc code = liftIO $ withSystemTempDirectory "emanote-mermaid" $ \tmpDir -> do
  let inputPath = tmpDir </> "diagram.mmd"
      outputPath = tmpDir </> "diagram.svg"
  writeFileBS inputPath (encodeUtf8 code)
  (exitCode, _stdout, stderr) <-
    readProcessWithExitCode mmdcBin ["-i", inputPath, "-o", outputPath] ""
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
