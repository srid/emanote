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

import Control.Monad.Logger (MonadLogger)
import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Emanote.Model.SData qualified as SData
import Emanote.Prelude (logE)
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

Per-diagram render failures log loudly via @logE@ but are intentionally
NOT fed into the per-note error list — that channel triggers a fatal
@exitFailure@ in 'checkBadMarkdownFiles', and a transient Chromium hiccup
shouldn't tank the entire site build. The failing block is still left
in place beneath a visible @Mermaid rendering failed:@ message so authors
notice the broken diagram in the rendered page.

A page can opt out of build-time rendering with @mermaid.static: false@
in its frontmatter (or in any ancestor @index.yaml@). With static rendering
disabled, code blocks are left intact for client-side rendering via the
@js.mermaid@ snippet.
-}
transformMermaidBlocks :: (MonadIO m, MonadLogger m) => Aeson.Value -> B.Pandoc -> m B.Pandoc
transformMermaidBlocks meta doc
  | not staticEnabled = pure doc
  | hasMermaidBlock doc = W.walkM renderBlock doc
  | otherwise = pure doc
  where
    staticEnabled :: Bool
    staticEnabled = SData.lookupAeson True ("mermaid" :| ["static"]) meta

hasMermaidBlock :: B.Pandoc -> Bool
hasMermaidBlock = getAny . W.query check
  where
    check (B.CodeBlock (_, classes, _) _) = Any (mermaidClass `elem` classes)
    check _ = mempty

renderBlock :: (MonadIO m, MonadLogger m) => B.Block -> m B.Block
renderBlock blk = case blk of
  B.CodeBlock (_, classes, _) code
    | mermaidClass `elem` classes ->
        runMmdc code >>= \case
          Right svg -> pure $ B.RawBlock (B.Format "html") svg
          Left err -> do
            logE $ "mermaid render failed: " <> err
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
      puppeteerConfig = tmpDir </> "puppeteer.json"
  writeFileBS inputPath (encodeUtf8 code)
  -- Puppeteer/Chromium needs help to start in restrictive build sandboxes:
  --   * GitHub Actions runners ship Chromium without the SUID-sandbox
  --     helper configured (mode 4755 / root-owned), so the default sandbox
  --     aborts launch with "The SUID sandbox helper binary was found, but
  --     is not configured correctly."
  --   * The Nix build sandbox has no /dev/shm and a read-only store; the
  --     crashpad handler then bombs with "chrome_crashpad_handler:
  --     --database is required" because it can't open its dump dir.
  -- mmdc only renders trusted local mermaid source — disabling these is safe.
  writeFileBS
    puppeteerConfig
    "{\"args\":[\
    \\"--no-sandbox\",\
    \\"--disable-setuid-sandbox\",\
    \\"--disable-dev-shm-usage\",\
    \\"--disable-gpu\",\
    \\"--disable-crash-reporter\",\
    \\"--no-zygote\",\
    \\"--single-process\"\
    \]}"
  -- `err` (not `stderr`) — Relude exports `stderr` as the standard handle,
  -- shadowing it would warn under -Wname-shadowing.
  (exitCode, _stdout, err) <-
    readProcessWithExitCode
      mmdcBin
      ["-i", inputPath, "-o", outputPath, "-p", puppeteerConfig]
      ""
  case exitCode of
    ExitFailure n ->
      pure . Left $ "mmdc exited " <> show n <> ": " <> T.strip (toText err)
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
