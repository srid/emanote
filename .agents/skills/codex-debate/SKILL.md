---
name: codex-debate
description: 'Run an automated codex⇄Claude debate to consensus — no round cap, no deadlock exit. Two explicit subcommands. `review` (also the bare/back-compat default) — codex (reviewer) critiques the current diff and a Claude subagent (author) fixes/disputes, looping until they agree. `answer` — Claude and codex each answer a freeform prompt in parallel, then cross-check until they agree, and a unified answer is returned. Use when the user types `/codex-debate`, asks to "have codex review this", "run the codex debate", "review this PR with codex", "argue this with codex until you agree", or passes a question to "have Claude and codex debate/answer until they agree".'
argument-hint: "review [<pr-number>] [--base <branch>] [--no-commit] [--no-comment] [--rationale <note>] [--context <note>]  |  answer \"<prompt>\""
---

# Codex ⇄ Claude debate

This skill runs an automated debate between **codex** and **Claude** that loops to
consensus with no round cap and no deadlock exit. It has **two modes**, selected by
an **explicit leading subcommand** — never by guessing from the argument's shape:

- **`review`** — codex reviews the current diff, a Claude author fixes/disputes,
  round after round until they agree, and the trail is **committed + posted to the
  PR** (a mutating, outward-facing mode). This is everything from
  [Review mode](#review-mode) down.
- **`answer`** — Claude and codex **each answer a freeform prompt in parallel**,
  then **cross-check each other** until both agree, and a **unified answer** is
  returned to you (read-only; plus a saved transcript). See
  [Answer mode](#answer-mode).

The two modes have **different side-effect contracts** (review mutates + writes to
the PR; answer is read-only), so the mode is chosen **explicitly**, not inferred
from whether the argument looks like a PR number or like prose. Inferring a
mutating action from prose shape is exactly the coupling this design avoids.

## Mode detection (do this first)

Look at the **first whitespace-delimited token** of `$ARGUMENTS`:

- **`answer`** → **answer mode**. The prompt is everything after the `answer`
  token. Jump to [Answer mode](#answer-mode); the review-mode steps do not apply.
- **`review`** → **review mode**. The remaining args are the review grammar
  (`[<pr-number>] [--base …] [--no-commit] [--no-comment] [--rationale <note>]
  [--context <note>]`). Continue with [Review mode](#review-mode).
- **No args, OR the first token is a number (a PR number) or a `--flag`** →
  **review mode** (the backward-compatible bare alias for the original
  `/codex-debate [<pr>] [flags]`, so existing callers like `/be-review` keep
  working). Continue with [Review mode](#review-mode).
- **Anything else** (freeform prose with no recognized subcommand) → **ambiguous**.
  Do **not** guess — ask the user to pick an explicit mode and stop, e.g.: "Did you
  mean `/codex-debate answer \"<your prompt>\"` (read-only) or `/codex-debate review
  [<pr>] [flags]` (mutating)?" Only the safe, backward-compatible review grammar
  auto-routes; prose never silently triggers a mode.

Both modes require Claude Code's **`Workflow` tool** (the engine). Under
codex/opencode runtimes the skill is inert.

<a id="review-mode"></a>
# Review mode — Codex ⇄ Claude review debate

Automate the back-and-forth you'd otherwise courier by hand: **codex** (the
reviewer) critiques the current change, a **Claude subagent** (the author)
fixes what it agrees with and disputes what it doesn't, codex re-reviews, and so
on — round after round, **until they reach consensus**. codex reviews from a
**warm session**: round 1 cold-starts the reviewer, and every later round
*resumes that same codex session* (`codex exec resume`), so codex carries its own
prior review and reasoning forward instead of reconstructing it from the diff +
rebuttal each round — when Claude disputes a finding, codex argues from its
original rationale. There is no round cap and
no "deadlock" surrender: a debate that quits without agreement defeats the
purpose, so the two sides keep arguing until one concedes. You stay out of the
middle: each round lands as its own commit whose
message carries the debate context (codex's findings + Claude's dispositions) so
the PR history reads as the debate, and the summary is **posted to the PR** as a
comment at the end.

## Why this shape

The two sides are asymmetric, and that asymmetry is the whole design:

- **codex** is CLI-invokable headlessly (`codex exec`, authed via ChatGPT), so it
  runs from a shell command.
- **Claude on a Max plan is *not* headless** — `claude -p` doesn't work with Max
  auth. But the **Workflow tool's `agent()` spawns Claude subagents through the
  harness**, not `claude -p`, so it works. That subagent is the author side.

So the debate runs as a Workflow: `agent()` is Claude, a Bash-invoked
`codex exec` is the reviewer, and the script couriers structured verdicts
between them and decides when they agree. Both sides are forced to emit
schema-constrained JSON, so consensus is detected in code, not by vibes.

**This skill requires Claude Code's `Workflow` tool** (it is the engine). Under
codex/opencode runtimes the skill is inert.

## Arguments

A leading `review` subcommand token, if present, is consumed by mode detection;
what remains is `[<pr-number>] [--base <branch>] [--no-commit] [--no-comment]
[--rationale <note>] [--context <note>]` (the bare alias passes the whole argument
string through unchanged). Parse:

- **`<pr-number>`** (optional): a PR to debate. If given, `gh pr checkout <n>`
  first and default the base to that PR's base branch. If omitted, debate the
  **current branch's** working-tree diff.
- **`--base <branch>`**: ref to diff against. Always a **remote-tracking ref**, never
  a stale local branch. Default: `origin/<PR base>` when a PR number is given, else
  the repo default branch as `git symbolic-ref --short refs/remotes/origin/HEAD`
  (e.g. `origin/master`) — used **as-is**, NOT stripped to local `master` (which
  can lag the remote). Fallback `origin/master`. Step 1 runs `git fetch origin`
  first so the ref is current. The workflow then resolves this to the **merge-base**
  of `base` and HEAD and diffs against that, so commits `base` gained since the
  branch forked aren't reviewed as part of this change.
- **`--no-commit`**: don't commit per round — leave all agreed changes
  uncommitted in the working tree for you to commit yourself. Default is to
  **commit each round** (see below).
- **`--no-comment`**: don't post the debate summary to the PR. By **default**, when
  a PR exists, the debate summary IS posted as a PR comment (see step 3). Pass
  this to suppress the outward-facing write and report in chat only.
- **`--rationale <note>`** (optional): the author's note on **deliberate** design
  decisions. Threaded into **both** sides — codex's round-1 review prompt (so it
  doesn't flag intentional choices as defects; its warm session carries the note
  across later rounds) and the Claude author's prompt **every round** (so it
  *disputes*, rather than "fixes", a finding that contradicts a deliberate choice).
  Mirrors `/lens-debate`'s `rationale`. Pull it from the PR/issue description, or the
  caller (`/be-review`) passes the change rationale straight through.
- **`--context <note>`** (optional): the **main-agent context** the Claude author
  should **inherit** — what this change is FOR (its task/intent and key decisions the
  orchestrator already holds). Injected into the author's prompt **every round** so it
  no longer reconstructs intent from the diff alone (`agent()` is one-shot and can't
  be resumed the way codex is, so re-injection is how it inherits at all). Given to
  the **author only**, not codex — codex stays an independent reviewer of the code,
  not the author's narrative. `/be-review` passes the task context through.

## Steps

### 1. Resolve context

- Determine `repoPath` (the worktree root, normally the cwd).
- **`git fetch origin`** so remote-tracking refs are current — the base is an
  `origin/...` ref, and a stale one would diff against the wrong tree.
- Resolve `base` per the rules above (a remote-tracking ref like `origin/master`).
- If a PR number was given, `gh pr checkout <n>` and confirm the branch.
- Confirm there is a non-empty diff: `git diff --stat <base>`. If empty, tell the
  user there's nothing to review and stop.
- **Preflight codex**: `codex login status`. If not logged in, stop and tell the
  user to run `codex login` (suggest the `!` prefix to do it in-session).

### 2. Run the debate Workflow

Invoke the **`Workflow` tool** pointing at this skill's committed script, passing
context through `args`:

```
Workflow({
  scriptPath: ".claude/skills/codex-debate/debate.workflow.js",
  args: {
    repoPath: "<worktree root>",        // also the per-worktree scratch dir root
    base: "<base branch>",
    commit: <false only if --no-commit>,
    skillDir: ".claude/skills/codex-debate",
    context: "<main-agent context the author inherits; omit/'' if none>",
    rationale: "<author's note on deliberate decisions; omit/'' if none>"
  }
})
```

The workflow runs in the background and notifies you when it completes. It
alternates `codex:roundN` and `claude:roundN` agents under a **Debate** phase —
the user can watch live via `/workflows`. Each Claude round edits the working
tree and (unless `--no-commit`) **commits exactly that round's changed files in
the same session** — one commit per round, with a message embedding the round's
codex findings and Claude's dispositions — never pushing or merging. (The commit
is no longer a separate `commit:roundN` agent: the author already has the tree
open, so it commits its own round.)

Ephemeral scratch (verdicts, the debate ledger) lives under the gitignored,
per-worktree `<repoPath>/.codex-debate/`, so **parallel debates in different
worktrees never collide** and the scratch never shows up in the diff codex
reviews. It returns:

```
{ status: "consensus" | "commit-incomplete" | "section-incomplete" | "reviewer-error",
  rounds, base, finalVerdict, filesChanged, commitGaps, sectionGaps, transcript,
  commentHeader,        // the comment's small deterministic header (badge + round count + effort + base)
  workDir, sectionGlob } // where the per-round section files live; cat them under the header (step 3)
```

(each `transcript[]` round also carries a `commit` SHA when that round committed;
`commitGaps` lists the round numbers whose author edited files but returned no
commit SHA — empty unless `status === "commit-incomplete"`; `sectionGaps` lists the
round numbers whose author left no disposition section file — empty unless
`status === "section-incomplete"`.)

There is one more, **earlier** terminus the workflow can return **before** the
debate loop even starts — `merge-base-error`. If `git merge-base <base> HEAD`
fails (a missing/typoed/stale base, or unrelated history), the diff scope can't be
trusted, so the workflow **aborts up front** rather than review the base branch's
drift as if this change made it. It returns a **different, smaller shape** — no
`commentHeader`, `workDir`, or `sectionGlob`, because no debate (and so no section
files) ever ran:

```
{ status: "merge-base-error", base, rounds: 0, transcript: [], finalVerdict: null,
  note }   // human-readable: which base failed and how to fix it (e.g. `git fetch`)
```
The debate is recorded as small Markdown section files under `<workDir>` — **two
per round**: `section-NNN-1-codex.md` (codex's verdict + findings) and
`section-NNN-2-claude.md` (the author's per-finding dispositions), zero-padded so
the `section-*.md` glob sorts in chronological order. **Each file is written by the
party that owns its content**: a Haiku writer renders codex's small *structured*
verdict to disk, and the **author writes its own dispositions directly** — the same
way codex writes its verdict to a path. That author-writes-its-own-file design is
deliberate: it keeps the author's *structured* return a **minimal ack**
(`filesChanged`/`commitSha`/`done`), so a big multi-finding narrative can never
overflow the structured-output encoding (the failure that used to crash the debate
on large diffs). The author's disposition file does triple duty — it's the author's
cross-round **memory**, the **rebuttal** codex reads next round (`codex-review.sh`
cats it straight into codex's prompt), and the **published comment** (step 3 cats
the section files under `commentHeader`). So the comment is still a **deterministic**
render — a shell `cat`, never re-improvised through an agent, nothing weak ever
retyping a large blob. codex is *not* a memory reader — it keeps its own warm
session and only ever reads the one rebuttal file.

- **consensus** — every finding codex raised is resolved (any severity — Claude
  fixed it or codex conceded the dispute). This is the *only* way the debate ends
  *normally*: it keeps running rounds until codex and Claude agree on every point,
  with no round cap and no deadlock exit. (The harness's own
  per-workflow agent backstop is the sole hard ceiling; if you ever need to stop
  a debate by hand, interrupt it via `/workflows` or `TaskStop`.)
- **commit-incomplete** — the debate *converged* (codex approved, nothing open),
  but a round's author edited files yet returned **no commit SHA**, so its
  in-session commit didn't land and the "one commit per round" contract broke for
  the round(s) in `commitGaps`. The edits are **not lost** — they stay in the
  working tree and the next reviewer diffs them against the base — but this is
  **not** a clean consensus: a human must reconcile the uncommitted round(s)
  (e.g. commit the outstanding tree) before relying on the per-round history. Do
  **not** report it as a plain consensus (see step 3).
- **section-incomplete** — the debate *converged*, but a round's author **skipped
  or under-filled its disposition section file** (`section-NNN-2-claude.md` missing,
  empty, or missing a backticked marker for an open finding, for the round(s) in
  `sectionGaps`). That file is the hole-free trail everyone draws on —
  the author's memory, the rebuttal codex reads next round, and part of the posted
  comment — so a miss means the published record has a gap. The code guards against
  feeding an empty rebuttal to codex (it warns and keeps the prior pointer), and the
  tree edits are still present, but this is **not** a clean consensus: a human must
  fill in the missing round(s) before trusting the per-round record. Do **not**
  report it as a plain consensus (see step 3).
- **merge-base-error** — an *up-front* abort, **before** any debate round runs:
  `git merge-base <base> HEAD` failed (missing/typoed/stale base, or unrelated
  history), so the review scope can't be trusted. The return carries a human-readable
  `note` (which base failed, how to fix it — e.g. `git fetch`) and **none** of the
  comment-assembly fields (`commentHeader`/`workDir`/`sectionGlob`), because no
  section files were ever written. Report the scope failure and **skip comment
  assembly/posting entirely** (see step 3); fix the base ref and re-run.
- **reviewer-error** — the one *abnormal* terminus: codex itself failed to
  produce a verdict (broken/unavailable CLI), so the workflow synthesized an
  error verdict and aborted rather than spin forever on a dead reviewer. This is
  **infrastructure failure, not a debate outcome** — `finalVerdict.summary`
  carries the failure detail (including how many attempts were made). Do **not**
  treat it as consensus (see step 3). **Transient failures are retried first:**
  `codex-review.sh` retries the `codex exec` invocation with linear backoff
  (default 3 attempts; tune via `CODEX_REVIEW_RETRIES` / `CODEX_REVIEW_BACKOFF`)
  and only synthesizes the reviewer-error verdict once every attempt comes back
  empty — so a single codex hiccup no longer sinks the round.

### 3. Present the result

**First branch on `status`.** If `status === "merge-base-error"`, the workflow
**aborted before any debate ran** — `git merge-base <base> HEAD` failed, so the
review scope couldn't be trusted. This return has **no** `commentHeader`,
`workDir`, or `sectionGlob` (no section files exist), so there is **nothing to
assemble**: do **not** run the posting block below. Report the scope failure —
surface the return's `note` (it names the failing base and the fix) — tell the
user to repair the base ref (e.g. `git fetch`, fix a typo'd/stale ref) and re-run,
and **skip the rest of this section**.

If `status === "reviewer-error"`, the debate did
**not** reach consensus — codex never produced a real verdict. Report it as a
**failure**, not a success: surface `finalVerdict.summary` (and the workflow log)
so the user sees codex was broken/unavailable, and tell them to fix codex (e.g.
`codex login`, check the CLI) and re-run. Do **not** post a consensus badge or a
`## Codex ⇄ Claude debate` PR comment for this path — there is no agreement to
report. Skip the rest of this section.

If `status === "commit-incomplete"`, the debate converged but at least one round
left its edits **uncommitted** (the round numbers are in `commitGaps`). Report it
as **converged-but-not-clean**: assemble and post the comment (see the posting
block below — `commentHeader` already shows a `⚠️` badge, not the consensus
check), then tell the user which round(s) are uncommitted and that the outstanding
tree must be committed before the per-round history can be trusted. Do **not** call
it a clean consensus.

If `status === "section-incomplete"`, the debate converged but at least one round's
author **skipped or under-filled its disposition section file** (missing, empty, or
omitting a marker for an open finding; round numbers in `sectionGaps`).
Report it as **converged-but-not-clean**: assemble and post the comment as usual
(the `⚠️` badge is already set), then tell the user which round(s) are missing
their disposition record and that the per-round history has a gap a human should
fill before trusting it. Do **not** call it a clean consensus.

Otherwise (`status === "consensus"`) report in chat (do **not** push or merge —
the per-round commits sit on the local branch for the human to review):

- The outcome — **consensus** — and how many rounds it took to get there.
- **The reviewer's reasoning effort** — sourced from the workflow's single
  `REASONING_EFFORT` constant (`xhigh` today), which is passed down to
  `codex-review.sh`'s `-c model_reasoning_effort` and into the comment header, so
  the published value and the config codex actually ran at share one home. Read
  it off the header rather than asserting it independently. State it so the depth
  of the review is on the record.
- `git log --oneline <base>..HEAD` (the per-round debate commits) and
  `git diff --stat <base>` so the user sees what the debate changed.
- A compact per-round summary — read it straight from the section files
  (`cat <workDir>/section-*.md`: each round's codex verdict, then the author's
  dispositions and commit SHA) so the convergence reads round by round. No need to
  re-derive it from `transcript`; the sections already render it.
- The agreed changes are committed per round on the local branch (or, under
  `--no-commit`, uncommitted in the working tree). The user reviews, then pushes
  / merges (or runs `/do --from post-implement`) when satisfied.
- **Post the debate summary to the PR (default).** When a PR exists and
  `--no-comment` was NOT passed, **assemble** the comment from the workflow's
  return — the small `commentHeader`, then the per-round section files `cat`-ed in
  glob order — and `gh pr comment <pr> -F <file>`:

  ```bash
  mkdir -p "$workDir"   # reviewer-error/--no-commit runs may not have created it
  {
    printf '%s\n' "$commentHeader"
    for f in "$workDir"/section-*.md; do printf '\n'; cat "$f"; printf '\n'; done
  } > "$workDir/comment.md"
  gh pr comment <pr> -F "$workDir/comment.md"
  ```

  (`$workDir` is the returned `workDir`, i.e. `<repoPath>/.codex-debate`; the
  `for`-loop guarantees a blank line between sections regardless of each file's
  trailing newline.) The result is the `## Codex ⇄ Claude debate` header (consensus
  badge, round count, the **reasoning-effort** note from the workflow's
  `REASONING_EFFORT` constant) followed by the per-round breakdown of codex's
  findings and the author's dispositions — the **same** section files the author
  read as memory and codex read as the rebuttal. So the comment is a
  **deterministic** shell concat of the record everyone drew on, not an
  LLM-improvised table — nothing weak ever retypes a blob. This is an outward-facing
  write — on by default because the whole point is to leave the review trail on the
  PR; `--no-comment` suppresses it.

<a id="answer-mode"></a>
# Answer mode — Codex ⇄ Claude answer debate

When the argument is a **freeform prompt** (not a PR number/flags), the skill
generalizes the same debate machinery from *reviewing a diff* to *answering a
question*. The shape is **symmetric**, not author⇄reviewer: **Claude and codex are
two equal peers**. They each answer the prompt **independently and in parallel**,
then **cross-check each other's answer** round after round — conceding where the
other is right, holding firm (with evidence) where it isn't — **until both agree**.
A final pass **synthesizes their two converged answers into one unified reply**,
which you present to the user along with a saved transcript.

Both peers are **codebase-aware but read-only**: each may read this repo (`git
diff/log`, read files, grep) to ground its answer, but neither edits anything —
codex stays under `--sandbox read-only` (kernel-enforced), and the Claude peer is
instructed not to write. Consensus is **schema-detected in code**: each side emits
a structured answer with an `agreesWithOther` boolean and an `objections` list, and
the loop ends only when **both** sides report no remaining disagreement. There is
**no round cap and no deadlock exit** — same as review mode.

## Steps

### A1. Resolve context

- Determine `repoPath` (the worktree root, normally the cwd).
- Capture the **prompt**: everything **after the `answer` subcommand token** (strip
  surrounding quotes). If it's empty, ask the user what they want answered and stop.
- **Preflight codex**: `codex login status`. If not logged in, stop and tell the
  user to run `codex login` (suggest the `!` prefix to do it in-session).
- No `git fetch` / base resolution / `gh pr checkout` here — answer mode doesn't
  diff a branch.

### A2. Run the answer Workflow

Invoke the **`Workflow` tool** pointing at this skill's committed answer script,
passing the prompt through `args`:

```
Workflow({
  scriptPath: ".claude/skills/codex-debate/answer.workflow.js",
  args: {
    repoPath: "<worktree root>",   // also the per-worktree scratch dir root
    prompt: "<the user's freeform prompt, verbatim>",
    skillDir: ".claude/skills/codex-debate"
  }
})
```

The workflow runs in the background and notifies you when it completes. It runs an
**Answer** phase (round 1: `claude:round1` and `codex:round1` in parallel), a
**Reconcile** phase (rounds 2+: each side cross-checks the other, in parallel,
round after round), and a **Synthesis** phase that merges the two agreed answers.
Watch live via `/workflows`. Ephemeral scratch (per-side answers, cross-check
files, per-round sections, the saved transcript) lives under the gitignored,
per-worktree `<repoPath>/.codex-debate/`, so parallel debates never collide. It
returns:

```
{ status: "consensus" | "reviewer-error" | "agent-error" | "synthesis-error" | "no-prompt",
  rounds, prompt, finalAnswer, transcriptPath, reasoningEffort, codexError }
```

- **consensus** — the only normal terminus: both sides agreed and then both
  **approved the synthesized candidate** (see the convergence note), and
  `finalAnswer` is that approved unified answer. `transcriptPath` points at the saved
  Markdown transcript (`.codex-debate/answer-<slug>.md`).
- **reviewer-error** — codex itself failed to produce an answer (broken/unavailable
  CLI) after retries; `codexError` carries the failure detail. Infrastructure
  failure, not a debate outcome.
- **agent-error** — one side died on a terminal API error after retries.
- **synthesis-error** — both sides DID agree, but the final synthesis pass produced
  no answer (the synthesis agent died or returned empty). Not a successful answer —
  report it as a failure (there is agreement on record, only the merge failed).
- **no-prompt** — the prompt was empty (shouldn't happen if A1 guarded it).

### A3. Present the result

- If `status === "consensus"`: present **`finalAnswer`** to the user as the answer
  — this is the unified reply both Claude and codex agreed on. State **how many
  rounds** it took to converge and that **codex answered at `reasoningEffort`**
  (read it off the return value). Point the user at the saved transcript
  (`transcriptPath`) for the full convergence trail; optionally `cat` the
  `.codex-debate/answer-section-*.md` files to show a compact per-round summary
  (each side's answer, what changed, remaining objections). This mode makes **no
  outward-facing writes** — no PR comment, no commits — it just answers.
- If `status !== "consensus"`: report it as a **failure**, not an answer. Surface
  `codexError` (for `reviewer-error`) or the workflow log so the user sees what
  broke, and tell them how to fix it (e.g. `codex login`) and re-run. Do **not**
  present a half-debate as if it were an agreed answer.

## Answer-mode safety & notes

- **Both peers read-only — but enforced ASYMMETRICALLY.** codex runs under
  `--sandbox read-only` (kernel-enforced, belt-and-suspenders with the prompt text —
  it reads arbitrary repo files and could be prompt-injected). The **Claude peer is
  only prompt-enforced**: the harness's `agent()` exposes no sandbox/tool restriction
  (the same is true of every Claude reviewer in `/lens-debate` and review mode), so
  Claude's read-only behaviour rests on instruction, not a kernel guard. A
  prompt-injected or mistaken Claude agent *could* in principle edit files or run a
  git write — answer mode does not, and cannot here, harden against that the way it
  does for codex. If that risk matters for a given prompt, run the debate in a
  disposable/read-only worktree. Treat the read-only guarantee as **hard for codex,
  best-effort for Claude.**
- **Warm codex session.** Round 1 cold-starts `codex exec`; every later round
  resumes the same session (`codex exec resume`) so codex cross-checks from its own
  prior answer rather than reconstructing it. The session id lives in the
  gitignored per-worktree `.codex-debate/` (a distinct `codex-answer-session.id`,
  so it never collides with review mode's session), degrading gracefully to a cold
  start if capture ever fails.
- **Symmetric convergence, schema-detected, candidate-confirmed.** Each side emits
  `agreesWithOther` + `objections`; a side counts as agreeing only when it sets
  `agreesWithOther:true` AND leaves no objection, so a stray objection can't be
  papered over by an over-eager boolean. Because the two run in parallel each round,
  a single mutually-agreeing round can be a **swap false positive** (Claude adopts
  codex's prior answer while codex adopts Claude's — both report agreement, but their
  current outputs are swapped and still differ), and they can keep swapping back and
  forth, so counting consecutive parallel agreements does **not** prove the current
  outputs match. The only sound test is to make both sides judge **one shared piece
  of text**. So when a round shows mutual agreement, the workflow synthesizes a single
  **candidate** from the two agreed answers and runs a **confirmation phase**: both
  sides review that *identical* candidate (without rewriting their own answer) and
  either approve it or object. Approval is on one fixed text both actually saw, so no
  swap is possible; if both approve, that candidate is the converged answer — already
  signed off by both debaters (which is also why `finalAnswer` is never unapproved
  synthesized text). If either objects, the candidate is dropped and the cross-check
  loop resumes with the objections folded in. No round cap, no deadlock exit.
- **Chat + saved transcript, no outward writes.** The unified answer is presented
  in chat and the full transcript is saved to the gitignored
  `.codex-debate/answer-<slug>.md`. Unlike review mode, answer mode never commits
  or posts to a PR.

## Safety & notes (review mode)

- **codex runs read-only — enforced, not just asked.** codex is invoked with
  `--sandbox read-only`, so the kernel sandbox blocks file writes and other
  state-mutating syscalls; the prompt's "don't write" instruction is belt-and-
  suspenders, not the only guard. This matters because codex reviews arbitrary
  diffs and could be prompt-injected by file contents. The only writes to the
  tree come from the Claude author rounds. (codex auto-falls-back to its bundled
  bubblewrap when the system one is absent, so read-only works in containers.)
  Resume rounds enforce the same read-only policy via `-c sandbox_mode=read-only`
  (the `resume` subcommand has no `--sandbox` flag) — same kernel guard, set
  through config instead of the flag.
- **Warm reviewer session.** Round 1 cold-starts `codex exec`; the runner records
  codex's session id (its `thread_id`, captured from the `--json` event stream)
  under the scratch dir and every later round `codex exec resume`s it, so codex
  retains its own prior review across rounds. The session id lives in the
  gitignored per-worktree `.codex-debate/`, so parallel debates never resume each
  other's sessions. If the id is ever missing (round-1 capture failed), a later
  round transparently cold-starts with the full prompt + rebuttal — graceful
  degradation, never a wedge.
- **Warm author (context, not session).** The Claude author can't be resumed the
  way codex is — `agent()` is one-shot and Claude isn't headless under Max auth,
  so there's no session id to carry forward. The equivalent is context, not state:
  each follow-up round the author **reads the per-round section files**
  (`cat .codex-debate/section-*.md`) — every prior round's codex findings and its
  own dispositions — so it builds on its last round rather than re-deriving the
  whole diff, and won't re-fix or re-litigate findings already settled. Each round
  writes two small files (a Haiku-rendered codex section and the author's own
  disposition section), so round N>1 always sees rounds 1..N-1; round 1 has none
  yet, so it's byte-identical to a cold start (and if no sections exist, the author
  falls back to the diff + verdict). The *same* sections compose the PR comment step
  3 posts and the rebuttal codex reads, so the author's memory, the published
  summary, and codex's rebuttal are one record. Crucially, the author **writes its
  own disposition section directly** — it never has to pour that narrative through a
  structured field — so its structured return stays a minimal ack and can't overflow
  on a large, many-finding diff (the failure this design replaced). The Haiku writer
  only ever renders codex's small *structured* verdict, so nothing weak retypes a
  large blob. codex stays on its own warm session and never reads the sections —
  only the one rebuttal file each round.
- **Inherited context, not just diff.** On top of that cross-round memory, when the
  caller passes `context` and/or `rationale` the author **inherits them in EVERY
  round's prompt** — the main-agent intent (what the change is FOR) and the
  deliberate-decision note. So even **round 1** reasons from the change's purpose
  rather than the diff alone, and the author *disputes* a finding that contradicts a
  deliberate choice instead of dutifully "fixing" it. The `rationale` also rides
  codex's round-1 prompt (see `codex-review.sh`), so the reviewer doesn't raise those
  intentional choices at the source; `context` is the author's alone (codex stays an
  independent reviewer of the code, not the narrative).
- **Commits, but never pushes or merges.** Each round is committed locally (unless
  `--no-commit`) so the PR history reads as the debate, but the skill never
  pushes or merges. Consensus means "both AIs agree on the committed code," not
  "ship it" — the human reviews the commits and pushes/merges.
- **Parallel-safe.** Ephemeral scratch (verdicts and the per-round section files,
  the author-written ones doubling as the rebuttal) lives under the gitignored,
  per-worktree `<repoPath>/.codex-debate/`, so debates on many worktrees run at once
  without clobbering each other — no shared `/tmp` paths, and each worktree's section
  files are its own.
- **Posts to the PR by default.** When a PR exists, the debate summary — the
  `commentHeader` followed by the per-round section files `cat`-ed together (step 3)
  — is posted as a PR comment (outward-facing write) unless `--no-comment` is passed
  — the point is to leave the review trail on the PR.
- **Runs to consensus — no cap, no deadlock exit.** The loop ends only when codex
  and Claude agree; it does not bail out at a round cap or declare a "deadlock," because
  a debate that quits without agreement is pointless. The two sides keep arguing
  until one concedes. The harness's own per-workflow agent backstop is the sole
  hard ceiling; interrupt via `/workflows` or `TaskStop` if you ever need to stop
  one by hand. **The one carve-out is *not* a deadlock exit:** a finding that is *not a
  code edit for this worktree* but a downstream / ship-phase / process gate (a companion
  repo pinning this repo's final post-review HEAD, a CI/release step, a cross-repo PR)
  cannot be satisfied during the review — it targets the *post*-gauntlet HEAD. When
  CLAUDE shows a finding is such a gate, codex marks it **resolved-and-deferred**
  (acknowledged, handed to the ship phase) instead of holding it open forever. The CODE
  debate still converges to consensus the normal way; this only stops the loop spinning
  on a process gate neither side can land mid-review. It is narrow by design — a genuine
  code defect CLAUDE simply dislikes is still argued to consensus, no exit. (This is the
  loop that once spun until a human killed it on a `@kolu/surface` cross-repo run.)

## Files

Shared:

- `scripts/codex-exec-lib.sh` — the sourced core both modes share: the read-only
  `codex exec`/`resume` invocation, warm-session resolve/persist, retry/backoff,
  thread-id capture, and the synthesized error-verdict fallback (via a caller hook).
  The two mode scripts source this and add only their own prompts + verdict shape.

Review mode:

- `debate.workflow.js` — the Workflow script (the loop + consensus logic).
- `scripts/codex-review.sh` — the review-specific invocation (arg parsing, the
  review prompts, the verdict schema/session file, the verdict-shaped error).
- `scripts/codex-verdict.schema.json` — the JSON Schema codex's verdict is constrained to.

Answer mode:

- `answer.workflow.js` — the Workflow script for the symmetric answer-debate
  (parallel answers → cross-check loop to agreement → synthesis).
- `scripts/codex-answer.sh` — the answer-specific invocation (arg parsing, the
  answer prompts, the answer schema/session file, the answer-shaped error).
- `scripts/codex-answer.schema.json` — the JSON Schema codex's answer is constrained to.

These are generated from `agents/.apm/skills/codex-debate/`; edit the source there and
run `just ai apm` to regenerate.

ARGUMENTS: $ARGUMENTS
