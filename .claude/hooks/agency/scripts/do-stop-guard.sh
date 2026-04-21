#!/usr/bin/env bash
# Prevents Claude from stopping while /do workflow is still running.
# Reads .do-results.json and blocks the stop if active == true.
# Safe default: if the file exists but can't be parsed, block (not approve).
results="$CLAUDE_PROJECT_DIR/.do-results.json"
if [ ! -f "$results" ]; then
  echo '{"decision":"approve"}'
  exit 0
fi
active=$(jq -r '.active // empty' "$results" 2>/dev/null) || active="parse_error"
case "$active" in
  true)
    echo '{"decision":"block","reason":"/do workflow still running — continue from where you left off. Check .do-results.json for current progress."}'
    ;;
  parse_error)
    echo '{"decision":"block","reason":"Could not parse .do-results.json — file may be corrupted. Check and fix it before stopping."}'
    ;;
  *)
    echo '{"decision":"approve"}'
    ;;
esac
