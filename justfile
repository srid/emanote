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

# Refresh the self-hosted Google Fonts bundle under _emanote-static/fonts/
fonts-update:
    {{nix_shell}} emanote/default/_emanote-static/fonts/update.sh
