# `tests/nix` — portable Playwright e2e shell

Standalone Nix bundle that provisions Node.js and a Playwright-compatible
Chromium for cucumber/Playwright e2e suites on NixOS or any Nix-only
host where `npx playwright install --with-deps` (which shells out to
`sudo apt-get`) cannot work.

The shell exports `PLAYWRIGHT_BROWSERS_PATH` and
`PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD=1`, so the npm `playwright` package
finds Nix-managed browsers instead of trying to download its own.

## Layout

- `shell.nix` — the actual shell. Self-contained: pins its own nixpkgs.
  The pin is constrained by **playwright-driver matching the npm
  `playwright` version your tests import** — currently 1.57.0.
  Diverging the two raises "browser revision X not found at
  `$PLAYWRIGHT_BROWSERS_PATH`" at launch.
- `mod.just` — a one-recipe just module that wraps
  `nix-shell shell.nix --run '<cmd>'` as `just e2e run "<cmd>"`.

## Adopting in a new repo

1. Drop this directory in (e.g. as `tests/nix/`).
2. In your top-level `justfile`:
   ```just
   mod e2e 'tests/nix/mod.just'
   ```
3. Define repo-specific test recipes that call `just e2e run "<cmd>"`.
   The cmd is whatever your suite needs inside the shell — typically
   `cd <test-dir> && npm install && <test-runner>`.

The path to the test directory is the consumer's choice — `tests/nix/`
deliberately doesn't bake in any assumption about where the cucumber
suite actually lives.

## Future: shared repo

This bundle and `nix/chrome-devtools/shell.nix` (the chrome-devtools
MCP shell) are both candidates for extraction into a shared repo when
a third consumer appears. Until then, each repo carries its own copy
and the version pin is local.
