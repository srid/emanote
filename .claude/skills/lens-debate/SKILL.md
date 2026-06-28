---
name: lens-debate
description: Run a structural-review debate between two lenses — lowy (volatility-based decomposition) and hickey (structural simplicity) — on the current diff. Each reviews independently, then they cross-examine every finding until they agree per-finding, and the agreed fixes are applied. Use when the user types `/lens-debate`, or asks to "have lowy and hickey review this", "run the lens debate", "debate this diff structurally", or "argue the structure of this PR until the lenses agree".
argument-hint: "[<pr-number>] [--base <branch>] [--max-rounds <n>] [--no-commit] [--no-apply] [--no-comment] [--with-police]"
---

# Lowy ⇄ Hickey lens debate

Two structural reviewers argue your change to a settled conclusion. **lowy**
(volatility-based decomposition — do boundaries encapsulate axes of change?) and
**hickey** (structural simplicity — are independent concerns complected, or one
thing fragmented?) each review the diff *independently*, then cross-examine
**every** finding from **both** reviews until they agree on each one. The agreed
`fix` findings are applied — each as its own commit — and the outcome is **posted
to the PR** as a comment. You stay out of the middle: the script couriers
schema-constrained dispositions between the lenses and decides when they agree.

This is the sibling of `/codex-debate`. Same engine (the `Workflow` tool), same
"both sides emit structured JSON so agreement is detected in code, not by vibes,"
same "commits but never pushes or merges." The difference is *who debates whom*.

## Why this shape

The structure was found by trial in #1109, and two parts of it are load-bearing:

- **Independent parallel review, then debate.** lowy and hickey review the diff
  *simultaneously and independently* — neither sees the other's findings before
  forming its own. A first cut fed hickey a *pre-curated* "lowy finding" to rebut
  and it concluded *drop* — framing bias. Running the reviews independently in
  parallel made hickey raise the same issue on its own, flipping the verdict to
  *fix*. **Curation biases the outcome; independent-then-debate does not.** So the
  lenses never trust a handed-down finding list — each reads the source itself.

- **Both lenses run on Opus** (overriding their `model: sonnet` frontmatter), as
  `/be` already requires for structural review.

- **The lowy lens runs Löwy's electricity probe.** Beyond the generic "where's
  the boundary?", the lowy reviewer must name the *receptacle* (the stable
  interface consumers plug into), the *volatile implementations* behind it,
  whether the thing is "electricity" (a domain-agnostic utility) or an app
  concern, and where a consumer is forced to "expose the wires." This is **not a
  second lens** — a separate voice would double-count lowy and reintroduce the
  framing bias above. It's the same volatility vote with a sharper probe that
  reliably pulls structural review out of abstraction and into "what plugs into
  what" (the abstraction-without-grounding failure mode a lens debate is prone
  to). It earned its keep on a live run (#1111).

## Why deadlock is not possible

Neither this skill nor `/codex-debate` has a deadlock exit — both run until
consensus, as many rounds as it takes. But the *reason* convergence is safe to
rely on is even stronger here. In `/codex-debate` the asymmetry is reviewer vs
**author**: Claude wrote the code and carries an authorship stake, so in
principle it could dig in and dispute a finding round after round (the loop
trusts good-faith concession to break the tie, and aborts only on reviewer
*infrastructure* failure).

Here both sides are **disinterested third-party lenses** applied to someone
else's diff. Neither authored the code; neither has anything to defend. Their
disagreements are not ego conflicts but framework-weighting differences ("is this
worth fixing in *this* PR?") about a shared question with a knowable answer. Two
good-faith analysts, each told to argue from the code and concede when the other
is right, **converge** — there is no fixed position to defend. So there is **no
deadlock exit**: the debate runs until consensus, as many rounds as it takes.

Three mechanics make that real rather than hopeful:

1. **Independent review** (above) removes the up-front framing bias.
2. **Settled findings lock.** The moment both lenses agree on a finding's
   disposition, it leaves the active set. The contested set is monotonically
   non-increasing — the debate can only shrink, never grow, so it can't oscillate
   a settled point back open.
3. **Sequential reveal.** Within a round lowy posts first and hickey answers
   lowy's *current* positions, so the two land together instead of chasing each
   other's stale positions.

`--max-rounds` (default **12**) is a pure safety backstop so a pathological
oscillation can't run unbounded — not a deadlock cap. Reaching it is reported as
`unresolved` (needs a human), never `deadlock`, and should essentially never
happen between two good-faith lenses.

**This skill requires Claude Code's `Workflow` tool** (it is the engine). Under
codex/opencode runtimes the skill is inert.

## Arguments

Parse `[<pr-number>] [--base <branch>] [--max-rounds <n>] [--no-commit] [--no-apply] [--no-comment] [--with-police]`:

- **`<pr-number>`** (optional): a PR to debate. If given, `gh pr checkout <n>`
  first and default the base to that PR's base branch. If omitted, debate the
  **current branch's** diff.
- **`--base <branch>`**: ref to diff against. Always a **remote-tracking ref**,
  never a stale local branch. Default: `origin/<PR base>` when a PR number is
  given, else the repo default branch via
  `git symbolic-ref --short refs/remotes/origin/HEAD` (e.g. `origin/master`),
  used **as-is**. Fallback `origin/master`. Step 1 runs `git fetch origin` first.
  The workflow resolves this to the **merge-base** of `base` and HEAD and diffs
  against that, so the base branch's drift since the fork isn't reviewed as ours.
- **`--max-rounds <n>`**: safety backstop on debate rounds. Default **12**. Not a
  deadlock cap (see above) — raise it freely.
- **`--no-commit`**: still apply the agreed fixes to the working tree, but leave
  them uncommitted for you to commit yourself. Default is to **commit each fix
  individually** (see below).
- **`--no-apply`**: skip the Apply phase entirely — the debate still settles every
  finding, but the agreed `fix` plans are **returned** (the `fixes` field) instead
  of implemented. For callers that want to review or re-validate the change
  requests against a different tree before applying them themselves. Implies
  nothing about commenting; the comment then records the fixes as "handed off".
  (`--no-commit` is moot under `--no-apply` — nothing is implemented, so nothing
  is committed.)
- **`--no-comment`**: don't post the debate summary to the PR. By **default**,
  when a PR exists, the summary IS posted as a PR comment (see step 3).
- **`--with-police`**: fold in `/code-police` as a third, **lower-weight voice**.
  It runs in the parallel review and *seeds* findings into the debate, but does
  **not** get a vote in consensus — only lowy ⇄ hickey decide agreement. Off by
  default (in #1109 its findings largely duplicated the lens findings).

## Steps

### 1. Resolve context

- Determine `repoPath` (the worktree root, normally the cwd).
- **`git fetch origin`** so the base remote-tracking ref is current.
- Resolve `base` per the rules above (a remote-tracking ref like `origin/master`).
- If a PR number was given, `gh pr checkout <n>` and confirm the branch.
- Confirm a non-empty diff: `git diff --stat <base>`. If empty, say there's
  nothing to review and stop.

### 2. Run the debate Workflow

Invoke the **`Workflow` tool** pointing at this skill's committed script, passing
context through `args`:

```
Workflow({
  scriptPath: ".claude/skills/lens-debate/debate.workflow.js",
  args: {
    repoPath: "<worktree root>",         // also the per-worktree scratch dir root
    base: "<base branch>",               // a remote-tracking ref, e.g. origin/master
    maxRounds: <n, default 12>,
    commit: <false only if --no-commit>,
    apply: <false only if --no-apply>,
    withPolice: <true only if --with-police>,
    rationale: "<optional author note on deliberate design decisions>",
    model: "<optional model override; defaults to opus>"
  }
})
```

The workflow runs in the background and notifies you when it completes. It runs
three phases the user can watch via `/workflows`:

- **Review** — `review:lowy`, `review:hickey` (and `review:code-police` with
  `--with-police`) in parallel, each independent.
- **Debate** — alternating `lowy:roundN` / `hickey:roundN` until every finding is
  agreed. Agreed findings drop out of each subsequent round. Agreement on a `fix`
  means both lenses agree on the disposition *and* the plan — if they both say
  `fix` but propose different changes, the finding stays open until the plans
  converge too (so Apply never picks one lens's plan arbitrarily).
- **Apply** — a single `apply:all` agent implements **every** agreed `fix` in one
  session and (unless `--no-commit`) commits each one individually, staging
  **exactly** that fix's changed files with a message carrying the debate context.
  One orientation for all fixes instead of a fresh implement+commit agent per
  finding. Skipped wholesale under `--no-apply` — the plans come back in `fixes`
  for the caller to apply.

When `rationale` is set, pull it from the PR/issue description (the deliberate
design decisions the author wants the lenses to respect, e.g. a deliberate
fail-open) so the lenses don't flag intentional choices.

Ephemeral scratch (commit-message files) lives under the gitignored, per-worktree
`<repoPath>/.lens-debate/`, so parallel debates in different worktrees never
collide and the scratch never shows up in the diff the lenses review. It returns:

```
{ status: "consensus" | "apply-incomplete" | "unresolved" | "clean",
  rounds, base, withPolice,
  settled,     // per-finding: id, origin, title, location, agreed disposition, plan, both reasonings
  unresolved,  // findings still contested at the backstop (empty on consensus)
  applied,     // [{ id, title, files, commit }] (empty under --no-apply)
  applyGaps,   // [{ id, reason }] agreed fixes that didn't cleanly land — empty unless status is "apply-incomplete"
  fixes,       // the agreed `fix` findings with converged plans — the caller's change requests under --no-apply
  reviews,     // each lens's independent findings
  history,     // per-round dispositions
  comment }    // the deterministically rendered PR comment body — post it VERBATIM (step 3)
```

- **consensus** — every finding settled (the normal outcome).
- **clean** — every lens found nothing worth raising.
- **apply-incomplete** — the lenses *converged*, but the Apply phase didn't land
  every agreed fix cleanly: a fix was **missing from the apply agent's output**
  (so we can't confirm it was applied) or, in commit mode, was **changed but
  returned no commit SHA** (its per-fix commit didn't land). The offending fixes
  are in `applyGaps`. Any edits present stay in the working tree, but this is
  **not** a clean consensus — surface the gap and reconcile it (re-apply or commit
  the outstanding fix) before relying on the per-fix history. Do **not** report it
  as a plain consensus.
- **unresolved** — the backstop was hit with findings still contested. Rare;
  needs a human. This is NOT a deadlock — the lenses simply didn't converge in
  the round budget; raise `--max-rounds` or adjudicate the listed findings.

### 3. Present the result

Report in chat (do **not** push or merge — the per-fix commits sit on the local
branch for the human to review):

- The outcome (`status`) and round count.
- `git log --oneline <base>..HEAD` (the per-fix commits) and `git diff --stat
  <base>` so the user sees what the debate changed.
- A per-finding table from `settled`: origin (lowy/hickey/police), title,
  location, agreed disposition (fix/drop), and the applied commit SHA for fixes.
- On any **unresolved** finding, surface both lenses' final positions plainly so
  the human can adjudicate — do not pick a winner yourself.
- **Post the debate summary to the PR (default).** When a PR exists and
  `--no-comment` was NOT passed, post the workflow's **deterministically rendered
  `comment`** verbatim — write it to a file and `gh pr comment <pr> -F <file>`:

  ```bash
  mkdir -p "$repoPath/.lens-debate"   # clean/all-drop/--no-commit runs never run the Apply commit step, so the dir may not exist yet
  printf '%s' "$comment" > "$repoPath/.lens-debate/comment.md"
  gh pr comment <pr> -F "$repoPath/.lens-debate/comment.md"
  ```

  The workflow returns `comment` already rendered — the
  `## [⚖️ Lowy ⇄ Hickey lens debate](https://kolu.dev/blog/hickey-lowy/)` header
  with the outcome badge and round count, the independent per-lens finding counts,
  the applied fixes (with commit SHAs), the agreed no-change observations, and any
  unresolved findings with both lenses' positions. Posting the returned string
  (rather than re-improvising a table) keeps the comment a **deterministic** render
  of the debate outcome. This mirrors `/codex-debate`; `--no-comment` suppresses it.

## Safety & notes

- **The lenses are read-only reviewers; only the Apply phase writes.** lowy and
  hickey never edit code — they only emit dispositions. The sole writes to the
  tree come from the single `apply:all` agent implementing the *agreed* fixes
  (one session, one commit per finding) — not one agent per fix.
- **Commits, but never pushes or merges.** Each agreed fix is committed locally
  (unless `--no-commit`) so the PR history reads as the debate's conclusions, but
  the skill never pushes or merges. Consensus means "both lenses agree on the
  disposition," not "ship it" — the human reviews the commits and pushes/merges.
- **No deadlock; bounded by a safety backstop.** The loop runs to consensus.
  `--max-rounds` only prevents a pathological unbounded run; reaching it is
  reported as `unresolved`, not deadlock.
- **Parallel-safe.** Ephemeral scratch lives under the gitignored, per-worktree
  `<repoPath>/.lens-debate/`, so debates on many worktrees run at once without
  clobbering each other.
- **Posts to the PR by default** (unless `--no-comment`) — the point is to leave
  the structural-review trail on the PR.

## Files

- `debate.workflow.js` — the Workflow script (parallel review + the
  lock-and-converge debate loop + the apply phase).

The lenses read `.claude/skills/{lowy,hickey}/SKILL.md` (and
`.claude/skills/code-police/SKILL.md` with `--with-police`) at runtime for their
frameworks.

This is generated from `agents/.apm/skills/lens-debate/`; edit the source there and run
`just ai::apm` to regenerate.

ARGUMENTS: $ARGUMENTS
