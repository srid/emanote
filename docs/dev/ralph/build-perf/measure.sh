#!/usr/bin/env bash
# Ralph build-time measurement helper.
# Usage (from repo root):
#   ./docs/dev/ralph/build-perf/measure.sh cold N    # N full clean builds
#   ./docs/dev/ralph/build-perf/measure.sh incr N    # N incremental builds (touch leaf)
#   ./docs/dev/ralph/build-perf/measure.sh ghcid N   # N ghci loads via `:quit`
#   ./docs/dev/ralph/build-perf/measure.sh all       # cold 3, incr 5, ghcid 3
#
# Output: each "row" run prints one line "RUN <kind> <i> <seconds>"; the
# script then prints "MEDIAN <kind> <n> <median_seconds>".
# Runs inside the nix dev shell.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../../.." && pwd)"
LEAF_FILE="emanote/src/Emanote/View/TaskIndex.hs"

# Return monotonic nanoseconds.
ns() { date +%s%N; }

# Median of newline-separated floats. Avoids requiring jq/datamash.
median() {
  python3 -c 'import sys, statistics; xs=[float(x) for x in sys.stdin.read().split() if x]; print(f"{statistics.median(xs):.3f}")'
}

run_cold() {
  local n="$1" i
  local results=()
  for ((i = 1; i <= n; i++)); do
    cabal clean >/dev/null 2>&1
    local t0
    t0=$(ns)
    cabal build all -v0 >/dev/null
    local t1
    t1=$(ns)
    local dt
    dt=$(awk -v a="$t0" -v b="$t1" 'BEGIN { printf "%.3f", (b - a) / 1e9 }')
    echo "RUN cold $i $dt"
    results+=("$dt")
  done
  printf "%s\n" "${results[@]}" | median | awk '{ printf "MEDIAN cold '"$n"' %s\n", $1 }'
}

# Append a no-op trailing newline then strip it to bump ABI minimally.
# Edits LEAF_FILE in a content-changing way that ghc will see as a real edit.
# Restores file at the end of each iteration (idempotent).
bump_leaf() {
  local marker
  marker="-- ralph-bump $(ns)"
  # Append marker as last line.
  printf '\n%s\n' "$marker" >>"$LEAF_FILE"
}
restore_leaf() {
  # Remove any trailing `-- ralph-bump ...` lines.
  if grep -q '^-- ralph-bump ' "$LEAF_FILE"; then
    python3 - "$LEAF_FILE" <<'PY'
import sys, re
p = sys.argv[1]
with open(p) as f: lines = f.readlines()
while lines and (lines[-1].strip() == "" or lines[-1].startswith("-- ralph-bump ")):
    lines.pop()
lines.append("")  # keep trailing newline
with open(p, "w") as f: f.writelines(lines)
PY
  fi
}

run_incr() {
  local n="$1" i
  local results=()
  # Ensure build is warm and clean state.
  cabal build all -v0 >/dev/null
  for ((i = 1; i <= n; i++)); do
    bump_leaf
    local t0
    t0=$(ns)
    cabal build all -v0 >/dev/null
    local t1
    t1=$(ns)
    local dt
    dt=$(awk -v a="$t0" -v b="$t1" 'BEGIN { printf "%.3f", (b - a) / 1e9 }')
    echo "RUN incr $i $dt"
    results+=("$dt")
  done
  restore_leaf
  cabal build all -v0 >/dev/null
  printf "%s\n" "${results[@]}" | median | awk '{ printf "MEDIAN incr '"$n"' %s\n", $1 }'
}

run_ghcid() {
  local n="$1" i
  local results=()
  # Warm up so subsequent runs measure interface-load only (the common case).
  echo ':quit' | cabal repl exe:emanote --flags=ghcid -v0 >/dev/null 2>&1 || true
  for ((i = 1; i <= n; i++)); do
    local t0
    t0=$(ns)
    echo ':quit' | cabal repl exe:emanote --flags=ghcid -v0 >/dev/null 2>&1
    local t1
    t1=$(ns)
    local dt
    dt=$(awk -v a="$t0" -v b="$t1" 'BEGIN { printf "%.3f", (b - a) / 1e9 }')
    echo "RUN ghcid $i $dt"
    results+=("$dt")
  done
  printf "%s\n" "${results[@]}" | median | awk '{ printf "MEDIAN ghcid '"$n"' %s\n", $1 }'
}

cd "$REPO_ROOT"

case "${1:-all}" in
cold) run_cold "${2:-3}" ;;
incr) run_incr "${2:-5}" ;;
ghcid) run_ghcid "${2:-3}" ;;
all)
  run_cold 3
  run_incr 5
  run_ghcid 3
  ;;
*)
  echo "usage: $0 {cold|incr|ghcid|all} [N]" >&2
  exit 2
  ;;
esac
