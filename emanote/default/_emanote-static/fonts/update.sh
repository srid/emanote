#!/usr/bin/env bash
# Refresh the self-hosted Google Fonts bundle.
#
# Usage: ./update.sh
#
# How it works:
#   1. Fetch Google Fonts' generated CSS (with a modern browser User-Agent
#      so the API returns woff2 URLs rather than fallback TTF).
#   2. Download each referenced woff2 blob into this directory.
#   3. Rewrite the CSS so every `url(https://fonts.gstatic.com/.../<hash>.woff2)`
#      becomes a local `url(./<hash>.woff2)`.
#
# About the filenames:
#   The woff2 files keep Google's original hash-based names
#   (e.g. `0QIhMX1D_JOuMw_LLPtLp_A.woff2`). Each hash maps to one specific
#   subset × weight × style slice of a font family. Preserving the hashes
#   means re-running this script only rewrites blobs whose contents
#   actually changed — a git-friendly update story.

set -euo pipefail
cd "$(dirname "$0")"

# Edit this URL to change the fonts ship with Emanote.
FONTS_URL='https://fonts.googleapis.com/css2?family=Lora:ital,wght@0,400..700;1,400..700&family=Mona+Sans:wght@400..700&family=Space+Mono:ital,wght@0,400;0,700;1,400;1,700&display=swap'

# Browser UA gets woff2 back from Google Fonts; the default UA returns TTF.
UA='Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36'

echo "Fetching CSS from Google Fonts..."
curl -sSA "$UA" "$FONTS_URL" > fonts.css

echo "Downloading woff2 blobs..."
grep -oE 'https://fonts\.gstatic\.com/[^)]+\.woff2' fonts.css | sort -u | while IFS= read -r url; do
  fname=$(basename "$url")
  if [[ -f "$fname" ]]; then
    echo "  (skip) $fname already present"
  else
    echo "  (get)  $fname"
    curl -sSL -o "$fname" "$url"
  fi
done

echo "Rewriting CSS to local paths..."
# Python handles the regex + in-place rewrite cleanly across sed variants.
python3 - <<'PY'
import re, pathlib
p = pathlib.Path('fonts.css')
css = p.read_text()
css = re.sub(r'https://fonts\.gstatic\.com/[^)]+/([^/)]+\.woff2)', r'./\1', css)
p.write_text(css)
PY

echo "Done. $(ls *.woff2 | wc -l) woff2 files, $(du -sh . | cut -f1) total."
