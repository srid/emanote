#!/usr/bin/env bash
# Refresh the cached Tailwind v4 browser CDN bundle.
#
# Plugins (typography, forms) and theme tokens are configured via the
# <style type="text/tailwindcss"> block emitted by `tailwindCssShim`, so
# this bundle only needs the core browser build.
set -euo pipefail
curl -L "https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4" \
  | sed 's|//# sourceMappingURL=/sm/[a-z0-9]*\.map||' \
  > tailwind.cdn.js
# The sourceMappingURL trailer points at /sm/<hash>.map which the live
# server doesn't route — stripping it silences the per-pageload 404
# without changing runtime behavior (DevTools just lacks the map).
