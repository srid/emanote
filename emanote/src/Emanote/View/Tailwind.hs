{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Tailwind v4 integration surface. Owns everything version-specific
about how Emanote talks to Tailwind — if Tailwind v5 lands with different
CLI flags or directive syntax, this is the one module that needs to change.
-}
module Emanote.View.Tailwind (
  tailwindBrowserConfig,
  themeRemapStyle,
  compileTailwindCss,
  generatedCssFile,
) where

import Control.Monad.Logger (runStdoutLoggingT)
import Data.ByteString qualified as BS
import Emanote.Prelude (log)
import NeatInterpolation (text)
import Paths_emanote qualified
import Relude
import System.FilePath ((</>))
import System.IO (hClose)
import System.Which (staticWhich)
import Text.Blaze.Html ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import UnliftIO (MonadUnliftIO)
import UnliftIO.Process (callProcess)
import UnliftIO.Temporary (withSystemTempFile)

generatedCssFile :: FilePath
generatedCssFile = "tailwind.css"

{- | The @\@theme@ block registering the 'primary' color token.

Aliased to the Tailwind blue palette at compile time; a per-site runtime
@\<style\>@ ('themeRemapStyle') overrides these to the site's configured
@template.theme@ palette, so theme color changes without recompiling.
-}
tailwindThemeBlock :: Text
tailwindThemeBlock =
  let aliases = primaryColorAliases "blue"
   in [text|
        @theme {
        $aliases
        }
      |]

{- | Config block for the Tailwind v4 browser CDN shim (dev mode).

Unlike the CLI path, the browser CDN only scans element @class@ attributes
for candidates — it does not look at inline @\<style\>@ content. So the
prod-mode pattern (hardcoded @var(--color-blue-\*)@ in @\@theme@ + per-page
@:root@ remap) leaves the chosen palette's CSS variables undefined in dev,
breaking live theme overrides.

Instead, bake the page's actual palette directly into @\@theme@ so Tailwind
sees the @var(--color-\<palette\>-\*)@ references in its own config input
and tree-shakes the right palette into the compiled CSS.

The browser build also explicitly rejects @\@plugin@ and @\@config@, so this
is @\@theme@-only. The CDN script itself provides the @\@import \"tailwindcss\"@.
-}
tailwindBrowserConfig :: Text -> Text
tailwindBrowserConfig paletteName =
  let aliases = primaryColorAliases paletteName
   in [text|
        @custom-variant dark (&:where(.dark, .dark *));
        @theme {
        $aliases
        }
      |]

{- | Full input CSS fed to the Tailwind v4 CLI (prod compile).

Includes @\@import@, @\@plugin@s, and the shared @\@theme@ block.
-}
tailwindCliInputCss :: Text
tailwindCliInputCss =
  [text|
    @import "tailwindcss";
    @plugin "@tailwindcss/typography";
    @custom-variant dark (&:where(.dark, .dark *));

    ${tailwindThemeBlock}
  |]

{- | Per-site @\<style\>@ that re-aliases @--color-primary-*@ to the user's
theme. The @id@ lets idiomorph match it across route switches so per-route
theme overrides take effect, and distinguishes it from the id-less
\@tailwindcss/browser@ CDN-injected style that needs @im-preserve@.
-}
themeRemapStyle :: Text -> H.Html
themeRemapStyle paletteName =
  let css = ":root {\n" <> primaryColorAliases paletteName <> "}\n"
   in H.style ! A.id "emanote-theme-remap" $ H.toHtml css

{- | CSS lines aliasing @--color-primary-N@ to @var(--color-\<palette\>-N)@,
one per scale level. Shared by the compile-time @\@theme@ block and the
runtime @:root@ remap so the two can't drift on scale levels.
-}
primaryColorAliases :: Text -> Text
primaryColorAliases paletteName =
  unlines
    [ "  --color-primary-" <> lvl <> ": var(--color-" <> paletteName <> "-" <> lvl <> ");"
    | lvl <- colorScaleLevels
    ]

colorScaleLevels :: [Text]
colorScaleLevels =
  ["50", "100", "200", "300", "400", "500", "600", "700", "800", "900", "950"]

tailwindBin :: FilePath
tailwindBin = $(staticWhich "tailwindcss")

{- | Compile the production @tailwind.css@ from the generated HTML files.

Writes a temp input CSS combining 'tailwindCliInputCss' with one @\@source@
line per generated path, then invokes @tailwindcss@ v4 with @--minify@.

The bundled JS modules under @_emanote-static\/js@ are also added as
sources: behaviors like the footnote popup apply Tailwind utility
classes via JS (@element.className = '...'@), so those class names
never appear in any generated HTML attribute. Without an explicit
source pointing at the JS, Tailwind's scanner doesn't see them and
the styles are dropped from @tailwind.css@ — popup, code-copy, etc.
all render unstyled in static-built sites.
-}
compileTailwindCss :: (MonadUnliftIO m) => FilePath -> [FilePath] -> m ()
compileTailwindCss cssPath genPaths = runStdoutLoggingT $ do
  log $ "Running Tailwind CSS v4 compiler to generate: " <> toText cssPath
  withSystemTempFile "emanote-tailwind-input.css" $ \inputPath h -> do
    jsSourcePath <- liftIO $ (</> "_emanote-static" </> "js") <$> Paths_emanote.getDataDir
    let sources =
          unlines (map (\p -> "@source \"" <> toText p <> "\";") genPaths)
            <> "@source \""
            <> toText jsSourcePath
            <> "\";\n"
        input =
          [text|
            ${tailwindCliInputCss}
            $sources
          |]
    liftIO $ do
      BS.hPut h (encodeUtf8 input)
      hClose h
    callProcess tailwindBin ["-i", inputPath, "-o", cssPath, "--minify"]
