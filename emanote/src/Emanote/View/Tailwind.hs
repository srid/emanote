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

import Control.Monad.Logger (runStdoutLoggingT)
import Data.ByteString qualified as BS
import Emanote.Prelude (log)
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
  let aliases = primaryColorAliases "blue"
   in [text|
        @import "tailwindcss";
        @plugin "@tailwindcss/typography";

        @theme {
        $aliases
        }
      |]

-- | Per-site @\<style\>@ that re-aliases @--color-primary-*@ to the user's theme.
themeRemapStyle :: Text -> H.Html
themeRemapStyle paletteName =
  let css = ":root {\n" <> primaryColorAliases paletteName <> "}\n"
   in H.style $ H.toHtml css

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

-- | The eleven Tailwind default color scale levels.
colorScaleLevels :: [Text]
colorScaleLevels =
  ["50", "100", "200", "300", "400", "500", "600", "700", "800", "900", "950"]

tailwindBin :: FilePath
tailwindBin = $(staticWhich "tailwindcss")

{- | Compile the production @tailwind.css@ from the generated HTML files.

Writes a temp input CSS combining 'tailwindInputCss' with one @\@source@ line
per generated path, then invokes @tailwindcss@ v4 with @--minify@.
-}
compileTailwindCss :: (MonadUnliftIO m) => FilePath -> [FilePath] -> m ()
compileTailwindCss cssPath genPaths = runStdoutLoggingT $ do
  log $ "Running Tailwind CSS v4 compiler to generate: " <> toText cssPath
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
