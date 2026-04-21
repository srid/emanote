#!/usr/bin/env bash
# Refresh the cached Tailwind v4 browser CDN bundle.
#
# Plugins (typography, forms) and theme tokens are configured via the
# <style type="text/tailwindcss"> block emitted by `tailwindCssShim`, so
# this bundle only needs the core browser build.
set -euo pipefail
curl -L "https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4" > tailwind.cdn.js
