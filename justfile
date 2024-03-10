notebook := "../docs;/Users/srid/code/nixos-asia-website/en@asia"

default:
    @just --list

# Run hoogle
docs:
    echo http://127.0.0.1:8888
    hoogle serve -p 8888 --local

# Run cabal repl
repl *ARGS:
    cabal repl --flags=ghcid {{ARGS}}

# Autoformat the project tree
fmt:
    treefmt

# Run the app using ghcid (with auto-reload / recompile)
# To run against a custom notebook:
#   just notebook=$HOME/code/mynotebook run
run:
  ghcid -c 'cabal repl exe:emanote --flags=ghcid' --warnings -T ":main -L {{notebook}} run --port=9010"

# Run tests (with auto-reload / recompile)
test:
    ghcid -c "cabal repl test:test --flags=ghcid" --warnings -T :main