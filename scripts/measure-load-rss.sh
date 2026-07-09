#!/usr/bin/env bash
# Start `emanote run` against a fixture, wait until loading + rendering settles,
# read VmHWM (peak RSS) from /proc, then kill the process. The metric that
# matches issue #66's reported "4.7 GB resident after loading" reading.
#
# Usage:
#   ./scripts/measure-load-rss.sh <emanote-bin> <fixture-dir> [--label NAME]
#
# Output: a single line "<label> peak-rss-kib=<KiB> rss-kib=<KiB>" on stdout, plus
# the full server log on stderr.
set -euo pipefail

BIN="${1:?missing emanote binary path}"
FIXTURE="${2:?missing fixture directory}"
LABEL="run"
shift 2
while [[ $# -gt 0 ]]; do
  case "$1" in
    --label) LABEL="$2"; shift 2 ;;
    *) echo "unknown arg: $1" >&2; exit 2 ;;
  esac
done

# Pick a free port.
PORT=$(nix run --extra-experimental-features 'nix-command flakes' nixpkgs#python3 -- -c \
  'import socket; s=socket.socket(); s.bind(("",0)); p=s.getsockname()[1]; s.close(); print(p)')

LOG=$(mktemp -t emanote-run-XXXXXX.log)

"$BIN" -L "$FIXTURE" run --port="$PORT" +RTS -s -RTS > "$LOG" 2>&1 &
PID=$!

cleanup() {
  if kill -0 "$PID" 2>/dev/null; then
    kill -TERM "$PID" 2>/dev/null || true
    wait "$PID" 2>/dev/null || true
  fi
}
trap cleanup EXIT

# Wait until the server reports it's listening (loading + initial template
# parse is then complete).
for _ in $(seq 1 240); do
  if nix run --extra-experimental-features 'nix-command flakes' nixpkgs#python3 -- -c \
    "import socket; s=socket.socket(); s.settimeout(0.2);
exit(0 if s.connect_ex(('127.0.0.1', $PORT)) == 0 else 1)" 2>/dev/null
  then
    break
  fi
  sleep 1
done

# Ema's live server starts listening on the port before union-mount has
# finished walking every file in the notebook layers — on a 4500-note
# notebook that "post-listen" loading phase takes ~30-60 seconds. Polling
# VmRSS until it stops growing across four consecutive samples is more
# reliable than a fixed sleep.
last_rss=0
stable=0
for _ in $(seq 1 60); do
  if [[ ! -d /proc/$PID ]]; then break; fi
  cur=$(awk '/^VmRSS:/{print $2}' "/proc/$PID/status")
  if [[ "$cur" -le "$last_rss" && "$cur" -gt 0 ]]; then
    stable=$((stable + 1))
    if [[ "$stable" -ge 3 ]]; then break; fi
  else
    stable=0
  fi
  last_rss="$cur"
  sleep 2
done
sleep 5

# Read peak RSS.
if [[ -d /proc/$PID ]]; then
  VMHWM=$(awk '/^VmHWM:/{print $2}' "/proc/$PID/status")
  VMRSS=$(awk '/^VmRSS:/{print $2}' "/proc/$PID/status")
  echo "$LABEL peak-rss-kib=$VMHWM rss-kib=$VMRSS"
else
  echo "$LABEL FAILED: process $PID exited prematurely" >&2
  cat "$LOG" >&2
  exit 1
fi

cat "$LOG" >&2
