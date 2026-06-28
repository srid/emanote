#!/usr/bin/env bash
#
# codex-review.sh — the canonical, deterministic codex invocation for the
# codex<->claude review debate (the `review` mode of /codex-debate). Runs the
# codex CLI as a READ-ONLY reviewer of the current working-tree state against a
# base branch, constrained to codex-verdict.schema.json, and writes the JSON
# verdict to <out-json>.
#
# This script owns only what is SPECIFIC to reviewing a diff: arg parsing, the
# warm/cold review prompt text, the verdict schema + session file, and the
# verdict-shaped error fallback. The shared codex-driving core (read-only exec/
# resume, retry/backoff, thread-id capture, session persistence) lives in
# codex-exec-lib.sh.
#
# Usage:
#   codex-review.sh <base-branch> <rebuttal-file|-> <out-json> [reasoning-effort] [rationale-file|-]
#
#   <base-branch>    branch to diff against (e.g. master)
#   <rebuttal-file>  path to a file holding CLAUDE's previous-round response — the
#                    author-written Markdown disposition section (its per-finding
#                    fixed/disputed/partial trail), NOT JSON. "-" on the first round
#                    (no rebuttal yet). Cat'd verbatim into codex's prompt below.
#   <out-json>       path the JSON verdict is written to (also echoed to stdout)
#   <reasoning-effort> codex model_reasoning_effort for this run; the debate
#                    workflow passes its REASONING_EFFORT constant here so the
#                    value has one home. Defaults to "xhigh" for standalone runs.
#   <rationale-file> path to a file holding the author's note on DELIBERATE
#                    decisions, or "-" for none. Injected into the round-1 (cold)
#                    review prompt so codex doesn't flag intentional choices as
#                    defects; codex's warm session carries it across later rounds.
#
# Notes:
#   * codex runs under `--sandbox read-only` (see codex-exec-lib.sh), which enforces
#     read-only at the kernel boundary (file writes and other state-mutating
#     syscalls denied), NOT merely by prompt text. codex reviews arbitrary diffs and
#     could be prompt-injected by file contents, so the read-only promise must be
#     enforced, not advertised.
#   * Always emits a schema-valid verdict on stdout, even if codex errors — a
#     synthesized error verdict (approved:false) so the loop never wedges.
#   * WARM SESSION: round 1 cold-starts codex and records its session id; every later
#     round resumes that same session (`codex exec resume <id>`) so codex retains its
#     OWN prior review + reasoning across rounds.
set -uo pipefail

base="${1:?usage: codex-review.sh <base-branch> <rebuttal-file|-> <out-json> [reasoning-effort] [rationale-file|-]}"
rebuttal_file="${2:?missing rebuttal-file (use - for none)}"
out="${3:?missing out-json path}"
# The debate workflow owns this value (its REASONING_EFFORT constant) and passes
# it down; "xhigh" is only the default for a standalone invocation of this script.
effort="${4:-xhigh}"
# Author's note on deliberate decisions (constant across rounds); "-" = none. Only
# the cold/round-1 prompt injects it — codex's warm session retains it after that.
rationale_file="${5:--}"

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
schema="$here/codex-verdict.schema.json"
# shellcheck source=codex-exec-lib.sh
source "$here/codex-exec-lib.sh"

# Pull CLAUDE's previous-round response (the author's Markdown disposition section),
# if any. Built as a plain string and injected below via a simple variable reference
# so any special characters in the text (backticks, $, ...) stay literal (heredoc
# expansion results are not re-scanned).
rebuttal=""
if [ "$rebuttal_file" != "-" ]; then
  if [ -s "$rebuttal_file" ]; then
    rebuttal="$(cat "$rebuttal_file")"
  else
    # A rebuttal was expected (path given, not "-") but the file is missing or
    # empty — the handoff broke. Proceed without it, but make the failure loud
    # so codex's responseToRebuttal isn't silently empty.
    echo "WARNING: expected rebuttal file '$rebuttal_file' is missing or empty; proceeding with no rebuttal this round." >&2
  fi
fi

rebuttal_block=""
if [ -n "$rebuttal" ]; then
  rebuttal_block="
This is a FOLLOW-UP round — you already gave your full review. Your job now is to
CLOSE OUT the findings already on the table, not re-scan the whole diff for more.
For each existing finding: verify CLAUDE's fix and mark it resolved, or address
CLAUDE's dispute (concede and mark resolved, or hold firm with specific reasoning
in responseToRebuttal). Raise a NEW finding ONLY if CLAUDE's changes this round
introduced it (a regression). Do NOT keep surfacing pre-existing issues you didn't
raise in round 1 — that prevents the debate from ever converging.

CLAUDE responded to your PREVIOUS review as follows:
$rebuttal
"
fi

# The author's note on DELIBERATE decisions, if supplied — injected into the COLD
# review prompt below so codex doesn't raise intentional choices as findings. Read
# from a file (the workflow writes it once) so multi-line notes with special chars
# survive intact, the same way the rebuttal is handled.
rationale=""
if [ "$rationale_file" != "-" ]; then
  if [ -s "$rationale_file" ]; then
    rationale="$(cat "$rationale_file")"
  else
    # A rationale was expected (path given, not "-") but the file is missing or
    # empty — the rationale:write handoff broke. Proceed without it (a missing
    # rationale degrades to a bare-diff review the IMPLEMENTOR still disputes from
    # its own inherited rationale block — it is a false-finding SUPPRESSOR, not a
    # correctness input, so we don't abort the round), but make the failure loud
    # so the round isn't silently mistaken for a rationale-aware review. Mirrors
    # the rebuttal warning above.
    echo "WARNING: expected rationale file '$rationale_file' is missing or empty; proceeding with no deliberate-decisions note this round (codex reviews the bare diff)." >&2
  fi
fi
rationale_block=""
if [ -n "$rationale" ]; then
  rationale_block="
The author flagged the following as DELIBERATE decisions. Do NOT raise them as
findings unless the reasoning itself is wrong — if it is, say specifically why:
$rationale
"
fi

# WARM SESSION. Round 1 (rebuttal_file == "-") cold-starts and resets any stale id;
# later rounds resume codex's own review session. Resolve the id first so the prompt
# below can lean on codex's retained context when warm.
session_id_file="$(dirname "$out")/codex-session.id"
[ "$rebuttal_file" = "-" ] && is_round1=1 || is_round1=
resume_id="$(codex_resolve_session "$session_id_file" "$is_round1")"

# Synthesize codex-review's error verdict shape when codex produces nothing after
# every attempt (called by codex_exec_round). reviewerError:true is the signal the
# workflow aborts the debate on.
synthesize_error_verdict() {
  local out="$1" tail_log="$2" attempts="$3"
  jq -n --arg log "$tail_log" --arg attempts "$attempts" '{
    approved: false,
    summary: ("codex produced no verdict this round after " + $attempts + " attempt(s). Tail of log: " + $log),
    findings: [],
    responseToRebuttal: "",
    reviewerError: true
  }' >"$out"
}

# Two prompts: a lean follow-up for the WARM (resume) path that leans on codex's
# retained context, and the full review prompt for the COLD path (round 1, or the
# fallback when no session id was captured). Unquoted heredocs: only $base,
# $rebuttal, $rebuttal_block, and (in the cold prompt) $rationale_block expand;
# their expansions are inserted literally (heredoc results aren't re-scanned), so
# special chars in $rebuttal / $rationale stay inert. The rationale rides the cold
# prompt ONLY — codex's warm session already retains it from round 1.
if [ -n "$resume_id" ]; then
  prompt="$(cat <<EOF
You are CODEX, continuing the SAME review session you started earlier — you still
have your own previous review and reasoning in context. The author ("CLAUDE") has
now responded to that review and changed the working tree.

The tree changed since your last turn, so re-inspect the CURRENT state (READ-ONLY —
do not modify, create, or delete anything, and run no git write command:
add/commit/push/stash/checkout):

    git diff $base       (committed + unstaged changes on this branch)
    git status --short   (untracked/new files — read those too; they aren't in the diff)

Ignore the debate's own scratch dir '.codex-debate/' if it appears.

CLAUDE responded to your previous review as follows:
$rebuttal

CLOSE OUT the findings already on the table — do NOT re-scan the whole diff for new
pre-existing issues you didn't raise before (that prevents the debate from ever
converging). For each existing finding (reuse its stable id): verify CLAUDE's fix
and mark it resolved, or address CLAUDE's dispute — concede (mark it resolved) or
hold firm with specific technical reasoning in responseToRebuttal. Raise a NEW
finding ONLY if CLAUDE's changes THIS round introduced it (a regression).

EXCEPTION to "hold firm": if CLAUDE shows a finding is NOT a code edit for THIS
worktree but a downstream / ship-phase / process gate (a companion repo pinning this
repo's final post-review HEAD, a CI/release step, a cross-repo PR), mark it RESOLVED —
acknowledged and DEFERRED to the ship phase. You cannot satisfy a ship-phase gate
mid-review, and holding it open deadlocks the debate forever; the review converges on
the CODE. This is ONLY for a genuine non-code/process gate, NEVER a code change CLAUDE
would simply rather not make — those you still hold firm on.

Return your updated review in the JSON schema:
  - findings: one entry per issue, each with severity and the stable id you used
    before. status=resolved once addressed (CLAUDE fixed it, OR you accept CLAUDE's
    reasoning); else open.
  - approved: true ONLY when EVERY finding is resolved, at every severity.
  - responseToRebuttal: address each of CLAUDE's disputes individually — concede or
    hold firm with specific, technical reasoning. Leave no dispute unanswered.
EOF
)"
else
  prompt="$(cat <<EOF
You are CODEX, a rigorous senior code reviewer. Review the changes in this branch
and give your honest, thorough feedback — exactly as you would on a serious PR.
You're in a debate with the author ("CLAUDE"), who will fix what they agree with
and push back, with reasons, on what they don't.

Inspect the change yourself (READ-ONLY — do not modify, create, or delete anything,
and run no git write command: add/commit/push/stash/checkout):

    git diff $base       (committed + unstaged changes on this branch)
    git status --short   (untracked/new files — read those too; they aren't in the diff)

Read every changed file plus enough surrounding code to judge it in context.
Ignore the debate's own scratch dir '.codex-debate/' if it appears.
$rationale_block
Give ALL your feedback in this pass — every issue worth raising, at EVERY severity
(blocking, major, minor, nit): correctness bugs, logic errors, silently swallowed
errors, unjustified fallbacks, security problems, and clear simplicity/efficiency
issues. Don't hold issues back for a later round, and don't limit yourself to
blockers — surface everything you see now. Cite file:line. (If the change is
genuinely clean, approving with no findings is fine — just never stay quiet about
a real issue to seem agreeable.)
$rebuttal_block
Return your review in the JSON schema:
  - findings: one entry per issue, each with a severity and a stable id (F1, F2, …)
    reused across rounds for the same issue. Set status=resolved once it is
    adequately addressed (CLAUDE fixed it, OR you accept CLAUDE's reasoning); else open.
  - approved: true ONLY when EVERY finding is resolved — all your feedback addressed
    at every severity, not just blockers. The review is not done while any issue you
    raised still stands open.
  - responseToRebuttal: when CLAUDE disputes a finding, address each dispute
    individually — concede (mark that finding resolved) or hold firm with specific,
    technical reasoning. Leave no dispute unanswered. Empty on round 1.

One EXCEPTION to "hold firm": if CLAUDE shows a finding is NOT a code edit for THIS
worktree but a downstream / ship-phase / process gate (a companion repo pinning this
repo's final post-review HEAD, a CI/release step, a cross-repo PR), mark it RESOLVED —
acknowledged and DEFERRED to the ship phase. You cannot satisfy a ship-phase gate
mid-review, and holding it open deadlocks the debate forever; the review converges on
the CODE. This is ONLY for a genuine non-code/process gate, NEVER a code change CLAUDE
would simply rather not make — those you still hold firm on.
EOF
)"
fi

# Drive codex for this round (retry/backoff, thread capture, error fallback) — the
# shared core does the work; this script supplied the prompt, schema, and shapes.
codex_exec_round "$schema" "$out" "$session_id_file" "$effort" "$resume_id" "$prompt"
