#!/usr/bin/env bash
set -eo pipefail
EMANOTE=${EMANOTE:-/home/toor/code/emanote/dist-newstyle/build/x86_64-linux/ghc-9.8.4/emanote-2.0.0.0/x/emanote/build/emanote/emanote}
export emanote_datadir=${emanote_datadir:-/home/toor/code/emanote/emanote/default}
CORPUS=${1:?corpus path}
RTS=${2:-}
PORT=${PORT:-$(( RANDOM % 10000 + 9000 ))}
TIMEOUT=${TIMEOUT:-600}
LOG=$(mktemp)
cd "$CORPUS"
$EMANOTE -L "$CORPUS" run --port "$PORT" $([ -n "$RTS" ] && echo +RTS $RTS -RTS) > "$LOG" 2>&1 &
PID=$!
READY=0
for i in $(seq 1 "$TIMEOUT"); do
  if ! kill -0 $PID 2>/dev/null; then echo "emanote died" >&2; tail -40 "$LOG" >&2; exit 1; fi
  if curl -s -o /dev/null --max-time 1 "http://localhost:$PORT/"; then READY=$i; break; fi
  sleep 1
done
[ "$READY" = 0 ] && { echo "timeout" >&2; kill $PID; exit 1; }
LOAD_RSS=$(awk '/VmRSS/{print $2}' /proc/$PID/status)
echo "READY_AFTER_S=$READY"
echo "LOAD_RSS_MB=$(awk -v r=$LOAD_RSS 'BEGIN{printf "%.0f", r/1024}')"
kill -INT $PID 2>/dev/null || true
sleep 2
kill $PID 2>/dev/null || true
wait $PID 2>/dev/null || true
echo "---LOG TAIL---"
tail -60 "$LOG"
