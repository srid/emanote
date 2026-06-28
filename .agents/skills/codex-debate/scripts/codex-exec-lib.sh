#!/usr/bin/env bash
#
# codex-exec-lib.sh — the shared, SOURCED core of the codex⇄claude debate scripts.
#
# Both modes of /codex-debate drive codex the same way; only WHAT they ask and the
# verdict SHAPE differ. This file is the single home for the part that is identical
# (and the part most volatile to codex CLI changes): driving codex headless and
# READ-ONLY, resuming its warm session, retrying transient failures with backoff,
# capturing the thread id, and — when codex produces nothing after every attempt —
# synthesizing a schema-valid error verdict so the debate loop never wedges.
#
# It is `source`d, not executed. The two callers (codex-review.sh, codex-answer.sh)
# own the parts that DIFFER: parsing their own args, building the prompt, choosing
# the schema + per-mode session-id file, and defining the verdict shape.
#
# Contract — a caller must:
#   1. source this file;
#   2. define a function  synthesize_error_verdict <out> <tail_log> <attempts>
#      that writes its own schema-valid error verdict (reviewerError:true) to <out>;
#   3. resolve the warm-session resume id with  codex_resolve_session <id-file> <round1?>
#      (so it can pick the warm vs cold prompt), then build $prompt;
#   4. run one round with  codex_exec_round <schema> <out> <id-file> <effort> <resume-id> <prompt>.
#
# Tunables (shared by both modes; names kept for back-compat):
#   CODEX_REVIEW_RETRIES  total attempts per round (default 3)
#   CODEX_REVIEW_BACKOFF  base seconds; attempt n waits n*base (default 5)

# Resolve the warm-session resume id for this round, and reset it on round 1.
#
#   * Round 1 (is_round1 == "1"): start a NEW session — drop any id left behind by
#     a previous debate in this worktree so we never resume a stale one. Echoes "".
#   * Later rounds: echo the persisted id (empty if none was captured — the caller
#     then cleanly cold-starts with the full prompt, never a wedge).
#
# Echoing (rather than setting a global) keeps the caller explicit: it captures the
# id, uses it to choose the warm vs cold prompt, and passes it back to
# codex_exec_round. Usage: resume_id="$(codex_resolve_session "$id_file" "$round1")"
codex_resolve_session() {
  local session_id_file="$1" is_round1="$2"
  if [ "$is_round1" = "1" ]; then
    rm -f "$session_id_file"
    return 0
  fi
  if [ -s "$session_id_file" ]; then
    cat "$session_id_file"
  fi
}

# One codex invocation: warm-resume when we have a session id (carries codex's own
# prior turn), else a cold start. `--json` emits a `thread.started` event carrying
# codex's thread_id (captured by codex_exec_round to resume next round); it does NOT
# change the verdict, which `--output-schema`/`-o` still write to "$out". `resume`
# has no `--sandbox` flag, so read-only is enforced there via `-c sandbox_mode` —
# the same kernel-enforced policy, set through config instead of the flag.
#
# Reads $resume_id, $schema, $out, $effort, $prompt from the enclosing
# codex_exec_round via bash's dynamic scope (they're `local` there).
_codex_run_once() {
  if [ -n "$resume_id" ]; then
    codex exec resume \
      -c sandbox_mode="read-only" \
      -c model_reasoning_effort="$effort" \
      --json \
      --output-schema "$schema" \
      -o "$out" \
      "$resume_id" "$prompt"
  else
    codex exec \
      --sandbox read-only \
      -c model_reasoning_effort="$effort" \
      --json \
      --output-schema "$schema" \
      -o "$out" \
      "$prompt"
  fi
}

# Run ONE debate round against codex and leave a schema-valid verdict in <out>
# (also echoed to stdout). Owns the out/log lifecycle, the retry/backoff loop, the
# thread-id capture, and the error-verdict fallback.
#
#   codex_exec_round <schema> <out> <session_id_file> <effort> <resume_id> <prompt>
#
# model_reasoning_effort is scoped to the debate here (via -c, from <effort>) rather
# than the user's global ~/.codex/config.toml — review/answer is the one place we
# always want codex thinking at full depth, regardless of their default.
#
# RETRY/BACKOFF. codex's CLI fails transiently often enough to matter (API hiccups,
# a spurious internal error) and writes no verdict — which would otherwise degrade
# the round to reviewer-error on a single bad roll. Retry with linear backoff,
# accepting the first attempt that writes a non-empty verdict to <out>. Only after
# every attempt fails empty do we synthesize the reviewerError verdict (via the
# caller's synthesize_error_verdict hook).
codex_exec_round() {
  local schema="$1" out="$2" session_id_file="$3" effort="$4" resume_id="$5" prompt="$6"
  local log="$out.log"

  # The out path lives under a per-worktree scratch dir (.codex-debate/); make sure
  # it exists before codex tries to write the verdict there.
  mkdir -p "$(dirname "$out")"

  local attempts="${CODEX_REVIEW_RETRIES:-3}"
  local backoff="${CODEX_REVIEW_BACKOFF:-5}"
  # Validate both as positive integers. Left unchecked, a non-numeric value makes the
  # arithmetic test error every iteration, so the loop would spin forever instead of
  # giving up. Fall back to the documented defaults (and clamp attempts to >=1)
  # loudly rather than wedge the headless debate on a typo'd override.
  if ! [[ "$attempts" =~ ^[0-9]+$ ]] || [ "$attempts" -lt 1 ]; then
    echo "WARNING: CODEX_REVIEW_RETRIES='$attempts' is not a positive integer; using 3." >&2
    attempts=3
  fi
  if ! [[ "$backoff" =~ ^[0-9]+$ ]]; then
    echo "WARNING: CODEX_REVIEW_BACKOFF='$backoff' is not a non-negative integer; using 5." >&2
    backoff=5
  fi

  local n=1 wait_s
  : >"$log"  # start each round fresh; attempts below APPEND so no failure's diagnostics are lost
  while :; do
    rm -f "$out"
    # Append (not truncate): when every attempt fails, the synthesized error
    # verdict's tail_log must reflect ALL attempts' diagnostics, not just the last.
    echo "=== attempt $n/$attempts ===" >>"$log"
    if ! _codex_run_once </dev/null >>"$log" 2>&1; then
      echo "codex exec exited non-zero (attempt $n/$attempts; see $log)" >&2
    fi
    # Success the moment codex writes a verdict: the kernel sandbox + --output-schema
    # make a non-empty "$out" a real, schema-valid verdict, not a partial.
    [ -s "$out" ] && break
    # Out of attempts — fall through to the synthesized error verdict.
    [ "$n" -ge "$attempts" ] && break
    wait_s=$(( backoff * n ))
    echo "codex produced no verdict (attempt $n/$attempts); retrying in ${wait_s}s..." >&2
    n=$(( n + 1 ))
    sleep "$wait_s"
  done

  if [ -s "$out" ]; then
    # Persist codex's session id so the NEXT round can resume this same warm session
    # (carrying codex's own prior turn). The successful attempt's `thread.started`
    # is the last one appended to the log; on a resume round it echoes the same id,
    # so overwriting is a harmless refresh. Failure to capture an id just means next
    # round cold-starts via the caller's fallback — not fatal.
    local sid
    # Extract the LAST thread_id in the log (one awk, no grep|tail|cut pipeline).
    # Splitting on '"', the value sits two fields AFTER the "thread_id" key field
    # (key, then ":", then value) — NOT a fixed column, since other quoted keys
    # (e.g. "type":"thread.started") precede it on the same JSON event line.
    sid="$(awk -F'"' '{for (i = 1; i < NF; i++) if ($i == "thread_id") sid = $(i + 2)} END {print sid}' "$log")"
    if [ -n "$sid" ]; then
      printf '%s\n' "$sid" >"$session_id_file"
    fi
  fi

  if [ ! -s "$out" ]; then
    # codex produced no verdict — hand off to the caller's shape-specific synthesizer
    # so the debate loop can surface the failure instead of hanging. reviewerError is
    # the machine-detectable signal the workflow aborts on: a broken/unavailable codex
    # is INFRASTRUCTURE failure, not substantive disagreement, so it must NOT spin the
    # loop forever.
    local tail_log
    tail_log="$(tail -c 2000 "$log" 2>/dev/null || true)"
    synthesize_error_verdict "$out" "$tail_log" "$attempts"
  fi

  cat "$out"
}
