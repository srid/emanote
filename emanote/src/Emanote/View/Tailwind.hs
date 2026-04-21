{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Tailwind v4 integration surface.

Owns everything version-specific about how Emanote talks to Tailwind:

  * 'tailwindInputCss' — the base input CSS (@import, @plugin, @theme).
  * 'themeRemapStyle' — the @\<style\>@ that per-site remaps @--color-primary-*@
    to the configured @template.theme@ palette.
  * 'compileTailwindCss' — shells out to the Tailwind v4 CLI for prod builds.
  * 'generatedCssFile' — filename the CLI writes to.

If Tailwind v5 lands with different CLI flags or directive syntax, this is
the one module that needs to change.
-}
module Emanote.View.Tailwind (
  tailwindInputCss,
  themeRemapStyle,
  compileTailwindCss,
  generatedCssFile,
) where

import Control.Monad.Logger (MonadLogger, logInfoN)
import Data.ByteString qualified as BS
import NeatInterpolation (text)
import Relude
import System.IO (hClose)
import System.Which (staticWhich)
import Text.Blaze.Html5 qualified as H
import UnliftIO (MonadUnliftIO)
import UnliftIO.Process (callProcess)
import UnliftIO.Temporary (withSystemTempFile)

generatedCssFile :: FilePath
generatedCssFile = "tailwind.css"

{- | Base Tailwind v4 input CSS.

Registers a 'primary' color token (all eleven scale levels) as CSS variables
aliased to the Tailwind blue palette at compile time. At runtime, a per-site
@\<style\>@ block ('themeRemapStyle') overrides the aliases to the site's
configured @template.theme@ palette, so a site's theme color changes without
regenerating the compiled CSS.
-}
tailwindInputCss :: Text
tailwindInputCss =
  [text|
    @import "tailwindcss";
    @plugin "@tailwindcss/typography";

    @theme {
      --color-primary-50:  var(--color-blue-50);
      --color-primary-100: var(--color-blue-100);
      --color-primary-200: var(--color-blue-200);
      --color-primary-300: var(--color-blue-300);
      --color-primary-400: var(--color-blue-400);
      --color-primary-500: var(--color-blue-500);
      --color-primary-600: var(--color-blue-600);
      --color-primary-700: var(--color-blue-700);
      --color-primary-800: var(--color-blue-800);
      --color-primary-900: var(--color-blue-900);
      --color-primary-950: var(--color-blue-950);
    }
  |]

-- | Per-site @\<style\>@ that re-aliases @--color-primary-*@ to the user's theme.
themeRemapStyle :: Text -> H.Html
themeRemapStyle themeName =
  let remap =
        unlines
          [ "  --color-primary-" <> lvl <> ": var(--color-" <> themeName <> "-" <> lvl <> ");"
          | lvl <- colorScaleLevels
          ]
      css = ":root {\n" <> remap <> "}\n"
   in H.style $ H.toHtml css

colorScaleLevels :: [Text]
colorScaleLevels =
  ["50", "100", "200", "300", "400", "500", "600", "700", "800", "900", "950"]

tailwindBin :: FilePath
tailwindBin = $(staticWhich "tailwindcss")

{- | Compile the production @tailwind.css@ from the generated HTML files.

Writes a temp input CSS combining 'tailwindInputCss' with one @\@source@ line
per generated path, then invokes @tailwindcss@ v4 with @--minify@.
-}
compileTailwindCss :: (MonadUnliftIO m, MonadLogger m) => FilePath -> [FilePath] -> m ()
compileTailwindCss cssPath genPaths = do
  logInfoN $ "Running Tailwind CSS v4 compiler to generate: " <> toText cssPath
  withSystemTempFile "emanote-tailwind-input.css" $ \inputPath h -> do
    let sources = unlines $ map (\p -> "@source \"" <> toText p <> "\";") genPaths
        input =
          [text|
            ${tailwindInputCss}
            $sources
          |]
    liftIO $ do
      BS.hPut h (encodeUtf8 input)
      hClose h
    callProcess tailwindBin ["-i", inputPath, "-o", cssPath, "--minify"]
