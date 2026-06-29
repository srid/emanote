#!/usr/bin/env bash
# Measure RSS of `emanote run` at: (a) ready, (b) after page hits, plus VmHWM.
set -eo pipefail
EMANOTE=${EMANOTE:-/home/toor/code/emanote/dist-newstyle/build/x86_64-linux/ghc-9.8.4/emanote-2.0.0.0/x/emanote/build/emanote/emanote}
export emanote_datadir=${emanote_datadir:-/home/toor/code/emanote/emanote/default}
CORPUS=${1:?corpus path}
RTS=${2:-}
PORT=${PORT:-$(( RANDOM % 10000 + 9000 ))}
TIMEOUT=${TIMEOUT:-600}
LOG=$(mktemp)
cd "$CORPUS"
RTS_ARGS=""
[ -n "$RTS" ] && RTS_ARGS="+RTS $RTS -RTS"
$EMANOTE -L "$CORPUS" run --port "$PORT" $RTS_ARGS > "$LOG" 2>&1 &
PID=$!
READY=0
for i in $(seq 1 "$TIMEOUT"); do
  if ! kill -0 $PID 2>/dev/null; then echo "DIED" >&2; tail -40 "$LOG" >&2; exit 1; fi
  if curl -s -o /dev/null --max-time 1 "http://localhost:$PORT/"; then READY=$i; break; fi
  sleep 1
done
[ "$READY" = 0 ] && { echo "TIMEOUT" >&2; kill $PID; exit 1; }
LOAD_RSS=$(awk '/VmRSS/{print $2}' /proc/$PID/status)
LOAD_HWM=$(awk '/VmHWM/{print $2}' /proc/$PID/status)
for p in / /topic00/n00000 /topic05/n00160 /topic15/n00480 /topic25/n00800 / ; do
  curl -s -o /dev/null --max-time 5 "http://localhost:$PORT$p" || true
done
sleep 3
HIT_RSS=$(awk '/VmRSS/{print $2}' /proc/$PID/status 2>/dev/null || echo 0)
HIT_HWM=$(awk '/VmHWM/{print $2}' /proc/$PID/status 2>/dev/null || echo 0)
printf "READY=%d LOAD_RSS=%d LOAD_HWM=%d AFTER_RSS=%d AFTER_HWM=%d\n" "$READY" "$(($LOAD_RSS/1024))" "$(($LOAD_HWM/1024))" "$(($HIT_RSS/1024))" "$(($HIT_HWM/1024))"
kill -INT $PID 2>/dev/null || true
sleep 1
kill $PID 2>/dev/null || true
wait $PID 2>/dev/null || true
