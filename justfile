notebook := "../docs"
nix_shell := if env('IN_NIX_SHELL', '') != '' { '' } else { 'nix develop -c' }

# List available recipes
default:
    @just --list

# Run hoogle
docs:
    echo http://127.0.0.1:8888
    {{nix_shell}} hoogle serve -p 8888 --local

# Run cabal repl
repl *ARGS:
    {{nix_shell}} cabal repl --flags=ghcid {{ARGS}}

# Autoformat the project tree
fmt:
    {{nix_shell}} pre-commit run --all-files

# Run the app using ghcid (with auto-reload / recompile)
# To run against a custom notebook:
#   just notebook=$HOME/code/mynotebook run
run:
    {{nix_shell}} ghcid -c 'cabal repl exe:emanote --flags=ghcid' --warnings -T ":main -L {{notebook}} run --port=9010"

# Run ghcid with log output to ghcid.log
ghcid:
    {{nix_shell}} ghcid --outputfile=ghcid.log -c 'cabal repl exe:emanote --flags=ghcid' --warnings

# Run tests (with auto-reload / recompile)
test:
    {{nix_shell}} ghcid -c "cabal repl test:test --flags=ghcid" --warnings -T :main

# Launch chrome-devtools MCP server (invoked by .mcp.json for Claude Code)
mcp-chrome-devtools *ARGS:
    nix-shell nix/chrome-devtools/shell.nix --run "chrome-devtools-mcp --headless=true --isolated=true {{ARGS}}"

# Run e2e tests (cucumber + playwright) against both live and static backends.
# Builds emanote once via Nix, then runs the suite twice with EMANOTE_MODE set.
# Playwright deliberately lives outside the Nix devshell (node_modules + chromium)
# because bundling browsers into Nix is fragile in CI — see tests/README.md.
# Omits `--with-deps` so local runs don't shell out to sudo apt-get; CI
# passes `--with-deps` in the workflow to install browser system libs on
# the ubuntu-latest runner.
test-e2e:
    #!/usr/bin/env bash
    set -euo pipefail
    BIN="$(nix build --no-link --print-out-paths .#default)/bin/emanote"
    cd tests
    npm ci
    npx playwright install chromium
    EMANOTE_BIN="$BIN" EMANOTE_MODE=live npm test
    EMANOTE_BIN="$BIN" EMANOTE_MODE=static npm test
