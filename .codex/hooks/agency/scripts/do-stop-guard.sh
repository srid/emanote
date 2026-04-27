#!/usr/bin/env bash
# Prevents the agent from stopping while /do workflow is still running.
# Reads .do-results.json and blocks the stop if active == "working".
# Output shape is cross-compatible with Claude Code and Codex: only
# `{"decision":"block","reason":"…"}` is emitted for the block case, and
# empty stdout is used for the approve case (Codex rejects `decision:"approve"`).
# Safe default: if the file exists but can't be parsed, block (not approve).
results="$CLAUDE_PROJECT_DIR/.do-results.json"
if [ ! -f "$results" ]; then
  exit 0
fi
active=$(jq -r '.active // empty' "$results" 2>/dev/null) || active="parse_error"
case "$active" in
  working)
    echo '{"decision":"block","reason":"/do workflow still running — continue from where you left off. Check .do-results.json for current progress."}'
    ;;
  parse_error)
    echo '{"decision":"block","reason":"Could not parse .do-results.json — file may be corrupted. Check and fix it before stopping."}'
    ;;
  *)
    ;;
esac
