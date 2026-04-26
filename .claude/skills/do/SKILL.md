---
name: do
description: Do a task end-to-end — implement, PR, CI loop, ship
argument-hint: "<issue-url | prompt> [--review] [--no-git] [--from <step>] [--review-model=<opus|sonnet|haiku>]"
---

# Do Workflow

Take a task and do it top-to-bottom: research, branch, implement, pass CI, open a PR, and ship. (Under `--no-git`, extend the working tree in place — no branch, commit, or PR.)

**Mostly autonomous.** Do NOT use `AskUserQuestion` at any point (except during the `--review` planning pause). Make sensible default choices and keep moving. If the user wants to skip specific steps, they can say so in the prompt — honor it.

## Arguments

Parse the arguments string: `[--review] [--no-git] [--from <step-id>] [--review-model=<opus|sonnet|haiku>] <task description or issue-url>`

The workflow is **forge-aware**: it auto-detects whether the repo lives on GitHub or elsewhere during the **sync** step (see Forge Detection). Only GitHub has an active code path today — Bitbucket/other forges gracefully skip PR-related steps. Tracking: [srid/agency#10](https://github.com/srid/agency/issues/10).

- `--review`: Pause after **research** for user plan approval via `EnterPlanMode`/`ExitPlanMode`, then continue autonomously. (hickey/lowy now runs post-implement on a concrete diff, so there's no plan-approval moment attached to that step anymore — the review point is pre-implement, before any code is written.)
- `--no-git`: Extend the working tree **in place** — do not create a branch, commit, push, or touch any PR. Research, implement, check, docs, police, fmt, and test all run; git-mutating steps (**branch**, **commit**, **create-pr**) are skipped. Use this when you have uncommitted local work and want the agent to build on it without taking over git state. Feedback from a Bitbucket user in [#26](https://github.com/srid/agency/issues/26).
- `--from <step-id>`: Start from a specific step (see entry points below)
- `--review-model=<model>`: Model to use for the **hickey+lowy** sub-agent invocations. Accepts `opus`, `sonnet`, or `haiku`. Defaults to `sonnet` — cheap enough to run on every task without thinking about cost. Pass `opus` when the task warrants deeper structural critique (large or architecturally significant diffs, refactors that cross module boundaries, work the user wants an extra-careful second pair of eyes on). Takes precedence over the `model: sonnet` in the hickey/lowy agent frontmatter via the `Agent` tool's `model` parameter.

## Results Tracking

Each step is bookended by two calls to the `scripts/do-results` script (in this skill's directory): `step-start <name>` before the work begins, and `step-end <status> <verification> [reason]` after verification. This is what keeps per-step timing accurate — collapsing both into a single end-of-step call produces zero-second durations and worthless timing tables. The script manages a JSON file with this schema:

```json
{
  "workflow": "do",
  "startedAt": "<ISO timestamp>",
  "active": "working",
  "status": "running",
  "forge": "github",
  "noGit": false,
  "steps": [
    {
      "name": "sync",
      "status": "passed",
      "verification": "...",
      "startedAt": "...",
      "completedAt": "..."
    }
  ]
}
```

- `forge` is set during **sync** (see Forge Detection below). One of `github`, `bitbucket`, `unknown`.
- `noGit` is `true` if the user passed `--no-git`. When set, git-mutating steps (**branch**, **commit**, **create-pr**) record status `skipped` with reason `"--no-git"`.
- Step `status` is one of `passed`, `failed`, or `skipped`. A `skipped` step must include a `reason` field explaining why (e.g., `"non-github forge: bitbucket"`, `"--no-git"`, `"no check command configured"`).

- `active` is a state enum, not a boolean. Set it to `"working"` when the workflow starts (**sync**), `"waiting"` when the agent is idle waiting for an external process (e.g., background CI), back to `"working"` when the external process returns, and `false` when the workflow ends (**done**). The stop hook uses this field: `"working"` blocks exits; `"waiting"` and `false` allow them.
- Set `status` to `"completed"` when **done** is reached, or `"failed"` if halted. This field is informational only.
- **Always use the `scripts/do-results` script** (in this skill's directory, alongside `scripts/steps/`) — never write the JSON file directly. Invoke with the full path (e.g. `.../skills/do/scripts/do-results ...`). Commands:
  - **Initialize**: `scripts/do-results init <forge> <noGit>` — creates the skeleton with a timestamp
  - **Start a step**: `scripts/do-results step-start <name>` — stamps `pendingStep` with the current UTC time. Call this **before** doing the step's work.
  - **End a step**: `scripts/do-results step-end <status> "<verification>" ["<reason>"]` — pops `pendingStep` and appends the completed step with `completedAt` set to the current UTC time.
  - **Record a step in one call** (advanced): `scripts/do-results step <name> <status> "<verification>" <startedAt> <completedAt> ["<reason>"]` — used by `scripts/steps/sync` where `startedAt` is captured in shell. Agent code should prefer `step-start` / `step-end`.
  - **Update top-level field**: `scripts/do-results set <field> <value>` (e.g., `set active waiting`, `set status completed`)
- **Bookend every step with `step-start` at the top and `step-end` at the bottom.** Calling `step-end` without a prior `step-start` is an error, and calling `step` with `now` for both timestamps collapses duration to 0 — neither pattern is allowed. The only exceptions: `sync` is recorded by `scripts/steps/sync` itself, and skipped steps (where duration is always 0 by definition) may use `step-start` followed immediately by `step-end` with status `skipped`.
- Do not run `date` yourself or guess timestamps — `do-results` resolves the current UTC time internally.

## Progress tracking

Drive Claude Code's native todo UI via the `TaskCreate` tool so the user sees a live checklist of the workflow. At the start of **sync** (or the chosen `--from` entry point), seed a task list with all 14 step names in order:

```
sync, research, branch, implement, check, docs, fmt, commit, hickey+lowy, police, test, create-pr, ci, done
```

At each step boundary, update task state **alongside** the `scripts/do-results` script call — they are not redundant. The JSON file is machine state for the stop hook; the task list is the human-facing UI. Miss either and the workflow is inconsistent.

Rules:

- **Flip to `in_progress` when a step starts, `completed` when it verifies.** One step `in_progress` at a time.
- **Retries stay `in_progress`.** If `check`, `test`, or `ci` loop through their retry budget, do **not** bounce the task state back to `pending` or flicker it — leave it `in_progress` until the step finally verifies (or the retries exhaust and the workflow fails).
- **`--from <step>` entry points**: still seed all 14 steps. Mark steps earlier than the entry point as `completed` immediately after seeding, so the checklist shows a consistent 14-item view regardless of entry point.
- **Skipped steps** (e.g. `branch`/`commit`/`create-pr` under `--no-git`, or PR steps on non-GitHub forges) go straight to `completed`. Record the skip with a back-to-back `scripts/do-results step-start <name>` / `scripts/do-results step-end skipped ... "<reason>"`; the task list just shows the step as done.
- **Failure**: if retries exhaust and the workflow halts, leave the failing step `in_progress`, mark `done` `completed` after the failure summary is written, and run `scripts/do-results set status failed`.

## Steps

### sync

Run the `scripts/steps/sync` script in this skill's directory, passing `true` or `false` for `--no-git`:

```
.../skills/do/scripts/steps/sync <noGit>
```

The script:

- Fetches `origin` and pins `origin/HEAD`
- If `--no-git` is **not** set and the branch is behind origin (ahead-count 0), fast-forwards with `git pull --ff-only`. Under `--no-git`, fetching happens but the working tree is not touched — uncommitted work is preserved.
- Prints the dirty-tree hint to stderr (no pause) when the tree is dirty and `--no-git` is not set:

  > _Dirty tree detected. Continuing will create a fresh branch on top of these changes. If you wanted the agent to extend your WIP in place without touching git, re-run with `--no-git`._

- Classifies the forge from `git remote get-url origin` — `github.com` → `github`, `bitbucket.` (covers `bitbucket.org` and self-hosted servers like `bitbucket.juspay.net`) → `bitbucket`, otherwise `unknown`.
- Calls `scripts/do-results init <forge> <noGit>` then `scripts/do-results step sync passed ...`.
- Prints `forge=<value>`, `branch=<value>`, `defaultBranch=<value>` on stdout for downstream steps.

**Only `github` has an active code path today.** Both `bitbucket` and `unknown` cause forge-dependent steps (PR creation, PR comments, PR edits, CI status) to skip gracefully. Bitbucket support is planned — see [srid/agency#10](https://github.com/srid/agency/issues/10).

**Verify**: Script exited 0 and printed a `forge=` line. `.do-results.json` exists and its `forge`/`noGit` fields match.

---

### research

Research the task thoroughly before writing code.

- If given a GitHub issue URL **and** `forge == github`, fetch with `gh issue view`. On non-GitHub forges, treat any issue-like URL as opaque context — use the prompt text as-is and do not attempt to fetch. (Bitbucket issue/Jira fetching is tracked in #10.)
- **Never assume** how something works. Read the code. Check the config.
- If the prompt involves external tools/libraries, use WebSearch/WebFetch.

**Delegation rule — keep the main context lean.** Before your third `Read` in this step, stop and delegate the rest via `Agent(subagent_type=Explore)`. Main-context reads are reserved for:

  (a) specific files the user named in the prompt,
  (b) `.apm/instructions/**` and files referenced from them,
  (c) verifying a specific file:line an Explore subagent cited — and only with `offset`/`limit`, never full-file.

Anything that smells like "map the codebase", "find all callers", "understand how X works across the repo" — delegate. The Explore subagent returns a file:line map; keep that map and reference it in later steps instead of re-reading. Use `Grep`/`Glob` before `Read`: if the question can be answered by searching, don't open the file.

**Verify**: Can articulate what needs to change, where, and why, with file:line citations drawn from the research map (not re-read in main context).

**If `--review`**: Use `EnterPlanMode` to present the approach for user approval:

- **Clarify ambiguities** first — ask via `AskUserQuestion` if anything is unclear. Don't guess.
- **High-level plan**: what to do and why, not implementation details. Include an **Architecture section** (affected modules, new abstractions, ripple effects).
- **Split non-trivial plans into phases** — MVP first, each phase functionally self-sufficient.

Use `ExitPlanMode` to present the plan. Once approved, continue autonomously to **branch**. Structural critique from hickey/lowy isn't available at this point — it runs post-implement on a concrete diff and surfaces as commits + a PR comment later.

---

### branch

**If `--no-git`**: Skip this step entirely with status `skipped` and reason `"--no-git"`. Stay on the current branch — do not create, commit, or push anything. Move to **implement**.

Detect the default branch: `git symbolic-ref refs/remotes/origin/HEAD`

1. Create a descriptive feature branch from `origin/<default>`

That's it — just the local branch. No commit, no push, no PR. The branch is pushed later in **commit**, and the PR is created in **create-pr** after all changes are done.

**Verify**: On a feature branch (not master/main).

---

### implement

If the task is a bug fix: write a failing test first (e2e or unit, whichever is appropriate), then fix the bug.

Otherwise: implement the planned changes. Prefer simplicity. Do the boring obvious thing.

**E2E coverage**: When the change introduces multiple user-facing paths (e.g., a dialog that appears under different conditions), write e2e scenarios for **each distinct path**. Enumerate the user-visible paths, then check that every one has a corresponding test.

**Verify**: Code changes match the planned approach. All distinct user-facing paths have test coverage.

---

### check

Read the project's instructions to find the check command — a fast static-correctness gate (e.g. `tsc --noEmit`, `cargo check`, `cabal build`, `mypy`, `dune build @check`). Run it.

This is the cheapest gate in the pipeline, so it runs first — fail fast on broken code before any downstream step does work over it. If no check command is documented, skip this step with a note.

**Verify**: Check ran without errors, or no command configured.
**If failed** (max 3 attempts): Fix the errors and re-run check. Do not fall back to **implement** — the agent is already in fix mode and the failure is local to just-written code.

---

### docs

Read the project's instructions to find which documentation files to keep in sync (e.g., README.md). Compare those files against changes in this PR.

If no documentation files are documented, skip this step with a note.

**Verify**: Docs match current code.
**If outdated** (max 3 attempts): Fix the outdated sections and re-verify.

---

### fmt

Read the project's instructions to find the format command (typically documented in a workflow instruction). Run it.

If no format command is documented, skip this step with a note.

**Verify**: Format command ran without error, or no command configured.

---

### commit

**If `--no-git`**: Skip with status `skipped` and reason `"--no-git"`. Move to **hickey+lowy**. The working-tree changes stay uncommitted — that is the point.

Create a NEW commit (never amend) with a conventional commit message for the primary implementation. Push to the feature branch with `git push -u origin <branch>` (sets upstream on first push).

This is the **primary feature commit**. Downstream **hickey+lowy** and **police** steps produce their own follow-up commits — one per finding or violation addressed — which keeps the PR history a readable progression of "what was built, then what was refined" rather than a single opaque squash.

**Verify**: `git log -1` shows a new commit on the feature branch, and it's pushed to remote.

---

### hickey + lowy

Invoke `hickey` and `lowy` as two **parallel sub-agents** via the harness's agent tool (`subagent_type: "hickey"` and `subagent_type: "lowy"`). On Claude Code this is the `Agent` tool. On Codex this is the sub-agent spawning tool for delegated work. Invoking `/do` is explicit authorization to run these two review agents; do not wait for a second user prompt before spawning them.

**Fallback, never skip.** If the harness cannot honor the requested review model in sub-agents, run hickey and lowy as sub-agents on the available model instead. If a sub-agent invocation fails for harness/tooling reasons before producing a review, retry that reviewer once; if it still cannot produce a sub-agent review, run that review in the main model by loading the reviewer skill against the same diff. This fallback is slower and uses more main-context budget, but it is still the `/do` hickey+lowy step. Do not replace it with an informal/manual review, and do not mark the step `skipped` because the requested model was unavailable.

**Why post-implement, not pre-implement.** Hickey's complecting critique and Lowy's volatility lens both bite harder on a concrete diff than on a plan sketch. Reviewing a plan tends to surface generic concerns; reviewing a real diff surfaces the specific interleavings and boundary misalignments that matter. Running here also means the review covers *everything* the diff contains — including whatever the plan glossed over and whatever drifted during implementation.

<use_parallel_tool_calls>
For maximum efficiency, invoke the `hickey` and `lowy` Agent tools **in parallel** rather than sequentially. You MUST use parallel tool calls: emit both `Agent` tool_use blocks (one with `subagent_type: "hickey"`, one with `subagent_type: "lowy"`) in a single response, with no other tool calls or text in that response.
</use_parallel_tool_calls>

Each `Agent` prompt must be self-contained (sub-agents do not inherit this conversation's context). Brief each one with:

- The full task prompt plus anything relevant that **research** uncovered (file paths, intended approach, key constraints)
- The scope to analyze: the actual diff, `git diff origin/HEAD...HEAD` — this is the same scope regardless of entry point (default or followup), since the branch at this point holds the primary feature commit (plus any cumulative followup commits) and no further work is pending

The sub-agent already knows to read its skill file and follow that methodology; don't re-state it in the prompt.

**Model override.** If the user passed `--review-model=<model>`, pass `model: "<model>"` in **both** `Agent` tool calls — this overrides the `model: sonnet` in the agents' frontmatter via the `Agent` tool's built-in `model` parameter. If the flag was not passed, omit the `model` parameter entirely so the agent definition's default (sonnet) applies. Accept only `opus`, `sonnet`, and `haiku`; reject anything else at argument-parse time with a one-line error, since a typo silently falling back to sonnet would hide a budget decision the user was trying to express.

After both reviewers return, **audit every `Defer` disposition before acting**. `/do` is not optimizing for minimal diff — it is optimizing for the simpler artifact landing in `master`. A PR that grows from 50 lines to 400 because hickey caught a real fragmentation bug is a *better* PR, not a worse one; the alternative is shipping the complected version and trusting a "broader refactor" follow-up that statistically never happens. **The default disposition is "Fix in this PR" — even when the fix grows the diff substantially.**

For every `Defer` the sub-agents returned, check the stated reason. Acceptable defers are narrow:

- The fix requires a change outside this repository's control (upstream library, sister repo, external API).
- The fix conflicts with concurrent in-flight work in another open PR on the same files.
- The fix requires a coordinated migration (data, schema, dependency upgrade) that must not be coupled to this PR's release timeline.

Reasons that are **never** acceptable — flip the disposition to "Fix in this PR" and apply:

- "Broader refactor", "wider scope", "out of scope", "follow-up refactor", "separate PR" — restatements of "the diff would grow," not reasons.
- "PR is getting big", "keep diff minimal" — `/do` does not constrain on diff size.
- "Pre-existing", "not introduced by this PR" — only acceptable when paired with one of the three narrow reasons above; otherwise the cleanup belongs at the lines this branch is already touching.
- Any `Defer` missing an issue link (e.g. "Defer — broader refactor" with no `#<issue>`) — the hickey/lowy skill files require issue links on defers, so a missing link means the sub-agent skipped the rule. Flip it unconditionally.

For each flipped finding, apply it in this PR using the per-commit rules below. After the audit, only **No-op** and surviving (genuinely-narrow) `Defer #issue` entries are surfaced in the PR comment (see **create-pr**) without code action — every other finding lands as a commit.

**Apply each "Fix in this PR" finding as its own commit** — do not batch multiple findings into one commit. A reviewer reading the PR's commit history should be able to read one "address hickey finding: decomplect viewportDimensions" commit at a time and follow the structural refinement as a sequence, not decode a grab-bag diff. For each finding in turn:

1. Apply the fix narrowly — only the lines that address this specific finding.
2. Run the project's format command (from **fmt** instructions) on the changed files, if one is configured.
3. `git add <changed files>` — stage only the files this fix touched.
4. `git commit -m "refactor(hickey): <short finding label>"` (or `refactor(lowy): …` depending on the lens). The body of the message should restate the finding in one line so the commit is self-explanatory in `git log`.
5. `git push` — push after each commit so the draft PR (once created) accumulates commits in real time. (The `-u` flag is only needed on the first push, which already happened in **commit**.)

**Under `--no-git`**: Skip the commit/push steps entirely. Apply fixes to the working tree and move on — the user will review the combined working-tree delta themselves. Record the step as passed with verification noting "--no-git: fixes applied to working tree, not committed."

**Verify**: Both hickey and lowy produced review output using their respective skills, either through sub-agents or the main-model fallback. Every finding has an action recorded (fix, defer, or no-op). Every "Fix in this PR" finding has a corresponding commit on the feature branch (check via `git log origin/HEAD..HEAD --oneline`), except under `--no-git`. No unactioned findings.

---

### police

Use `git diff origin/HEAD...HEAD --name-only` to check if the PR contains code changes. If all changed files are documentation-only (e.g., `.md`, `.txt`, `README`, docs/) — skip this step with a note.

Otherwise, invoke the `/code-police` skill via the Skill tool. It runs three passes: rule checklist, fact-check, and elegance (which delegates to `/simplify` when available).

When `/code-police` asks about scope: **changes in the current branch/PR only**.

**Commit each violation fix individually.** The same rule as **hickey + lowy**: PR history is the story of the work, and a reviewer should see one commit per rule violation or elegance refinement, not a lump "police pass" commit covering eight unrelated things.

For each violation reported by `/code-police` (across all three passes), in turn:

1. Apply the fix for that one violation — scope the edit tightly.
2. Run the project's format command on changed files, if configured.
3. `git add <changed files>` — stage only this fix.
4. Commit with a conventional prefix identifying the pass and rule:
   - Rules pass: `fix(police): <rule-id> — <short description>` (e.g. `fix(police): no-dead-code — remove commented-out fallback`)
   - Fact-check pass: `fix(police): fact-check — <short description>` (e.g. `fix(police): fact-check — propagate error from loader`)
   - Elegance pass (`/simplify`-applied or inline-loop-applied): `refactor(police): elegance — <short description>`
5. `git push`.

For the elegance pass specifically: `/simplify` applies fixes in batches across three lenses (reuse, quality, efficiency). Commit each distinct refactor as a separate commit — do not roll them into one "elegance" commit. If a lens produces multiple independent changes (two reuse-via-helper refactors in different files, say), those are separate commits too.

**Under `--no-git`**: Skip the commit/push steps. Apply fixes to the working tree and continue. The user reviews the combined delta.

**Verify**: All 3 passes clean ("All clear"). Under `--no-git`, the tree reflects the fixes; otherwise `git log origin/HEAD..HEAD --oneline` shows one commit per violation addressed.
**If violations found** (max 3 attempts): Fix the violations (one commit per fix, as above) and re-invoke `/code-police`.

---

### test

Read the project's instructions to find the test command and strategy. Run only the tests relevant to the code paths changed in this PR.

Use `git diff origin/HEAD...HEAD --name-only` to identify changed files and determine which tests are relevant.

If changes are purely internal with no user-facing impact, unit tests may suffice — skip e2e if no relevant scenarios exist. If no test command is documented, skip with a note.

**Verify**: Tests pass (exit code 0), or no relevant tests to run.
**If failed** (max 4 attempts): Analyze the failure. If flaky, re-run. If real: fix → go to **fmt**, then retry.

---

### create-pr

**If `--no-git`**: Skip with status `skipped` and reason `"--no-git"`. There is no PR to create. Proceed to **ci**.

**If `forge != github`**: Skip with status `skipped` and reason `"non-<forge> forge: <forge>"`. (Bitbucket `bkt pr edit` wiring is tracked in #10.) Proceed to **ci**.

**If `forge == github`**:

Check whether a PR already exists for this branch (`gh pr view`).

**If no PR exists** (first run, normal path):

1. Create a draft PR: `gh pr create --draft`

   **MANDATORY**: Load the `forge-pr` skill (via Skill tool) BEFORE writing the PR title/body.

2. **Post hickey/lowy results**: Post the hickey and lowy analysis as a PR comment using `gh pr comment` with a `## [Hickey/Lowy](https://kolu.dev/blog/hickey-lowy/) Analysis` header (the heading links to the blog post explaining the two lenses, mirroring how the final step status comment links `/do` to the agency repo). Always post when the steps ran, even if all findings are deferred or out of scope — reviewers should see the structural analysis.

   **Format the comment with a leading findings ledger.** Compose a single table from both sub-agents' Actions sections — one row per finding — so a reviewer can see disposition at a glance without parsing paragraphs. Put each lens's prose underneath as rationale:

   ```md
   ## [Hickey/Lowy](https://kolu.dev/blog/hickey-lowy/) Analysis

   | # | Lens   | Finding                                  | Disposition       |
   |---|--------|------------------------------------------|-------------------|
   | 1 | Hickey | viewportDimensions complects two roles   | Fixed in this PR  |
   | 2 | Lowy   | useViewport encapsulates ghost concern   | Deferred [#123]   |

   ### Hickey rationale
   <prose from the hickey sub-agent>

   ### Lowy rationale
   <prose from the lowy sub-agent>
   ```

   The Disposition cell mirrors the sub-agent's Actions disposition verbatim — **Fixed in this PR**, **Deferred [#N]** (linked), or **No-op** (deletion-only / subsumed by another finding). The Finding cell is the short bolded label the sub-agent emits at the start of each Actions entry. If both lenses produced zero findings, write a one-line "No findings — analysis below" instead of an empty table.

**If PR already exists** (followup runs, `--from` entry points):

Re-check the PR title/body against current scope. If scope changed, update via `gh pr edit` per the `forge-pr` skill.

**Surface deferred hickey/lowy findings**: If the hickey or lowy steps produced any **"Defer `#issue`"** actions, append a `> **Deferred:** #123, #124` line to the PR body (via `gh pr edit`) so reviewers see the outstanding structural debt. These are easy to miss in a PR comment — the description is what reviewers actually read.

**Why this runs before `ci`**: The draft PR is the canonical home for CI status. Opening it before CI runs means CI checks land directly on the PR, reviewers see the run history as it happens, and a failing run doesn't leave an orphaned branch with red statuses and no PR to explain them. If retries exhaust in **ci**, the draft PR remains as the artifact of the failed attempt — visible, reviewable, and ready to resume via `--from ci-only`.

**Verify**: Draft PR exists (`gh pr view` succeeds), PR title/body matches the delivered scope, hickey/lowy findings posted if any, and any deferred issues are linked in the body.

---

### ci

Read the project's instructions to find the CI command and verification method. Run CI with `run_in_background: true` if the command takes more than a few seconds.

**Never pipe CI to `tail`/`head`**, and **never append `2>&1`** — background mode captures both streams.

**Active state**: Before waiting for background CI, run `scripts/do-results set active waiting`. When CI returns (success or failure), run `scripts/do-results set active working` before proceeding. This lets the stop hook allow graceful exits while the agent is idle.

CI commands are typically local (e.g. `nix flake check`, `just ci`, `make ci`) and are forge-independent — **run them regardless of forge**. Only the *verification method* may be forge-specific: if the project's instructions describe verification via `gh` commit-status checks and `forge != github`, fall back to exit code + command output for verification on non-GitHub forges, and note this in the step record. (Bitbucket `bkt pr checks` wiring is tracked in #10.)

**Verify**: Use the verification method described in the project's instructions (e.g., checking commit statuses on GitHub, reading CI output elsewhere). If no CI command is documented, skip with a note. **The CI result must cover `HEAD`.** Before recording the step as passed, compare the commit SHA that CI ran against with `git rev-parse HEAD`. If they differ (e.g., a commit was pushed after CI started — whether from a fix retry, user-requested changes, or any other source), re-run CI against the current HEAD. CI passing on a stale commit does not satisfy verification.

**On failure** — read logs or output to diagnose.

**Flaky vs real**: A test is flaky only if it **passes on a subsequent retry**. Consistent failure = real bug. Before retrying, read the failing test code to judge if the failure pattern is inherently flaky (race conditions, timing, async waits).

**If flaky** (max 3 retries): Retry just the failing step.
**If real bug** (max 5 fixes): Fix → **fmt** → **commit** → retry CI. Under `--no-git`, drop **commit** from the loop (Fix → **fmt** → retry CI). The draft PR already exists — subsequent pushes update it automatically, no re-run of **create-pr** needed.
**If retries exhausted**: Set workflow status to `"failed"`, skip to **done**. The draft PR stays open as the record of the failed attempt.

---

### done

Present a summary of all steps with their verification status. If any step has a non-success status, retry it (max 3 attempts from done). If still failing after retries, set `status: "failed"`.

`"completed"` requires **all steps `passed`**, with two exceptions that count toward completion:

1. A step `skipped` with `reason` beginning `"non-<forge> forge:"` (detected forge isn't GitHub).
2. A step `skipped` with `reason` `"--no-git"` (user opted out of git operations).

A `failed` step always blocks `"completed"`. No redefining "passed," no footnote caveats. Update via `scripts/do-results set status completed` or `scripts/do-results set status failed` accordingly.

#### Timing summary

Run `scripts/steps/done` in this skill's directory. The script reads `.do-results.json` and emits:

1. A markdown timing table (step, status, duration, verification), with any step that took ≥30% of total time shown in **bold**.
2. A total wall-clock line (`startedAt` of first step → `completedAt` of last step).
3. A `**Slowest step**:` line.
4. A `<<<FACTS ... FACTS` block with machine-readable summary data (`totalSeconds`, `slowestStep`, `slowestSeconds`, `dominantSteps`, `skippedSteps`, `failedSteps`) — use this to compose optimization suggestions below.

Do not compute durations yourself — the script handles all timestamp arithmetic.

#### Optimization suggestions

Read the `FACTS` block the `done` script emitted and generate 2–4 concrete suggestions for reducing time-to-completion in future runs. Base these on the actual timing data — for example:

- If **ci** dominates: suggest `--from ci-only` for re-runs, or note which CI sub-step was slowest
- If **research** was slow: suggest pre-reading relevant code before invoking `/do`
- If **test** had retries: note the flaky test and suggest hardening it
- If **police** required fix iterations: note which pass caught issues (rules/fact-check/elegance)
- If **implement** was the bottleneck: suggest breaking the task into smaller PRs

Be specific to this run's data, not generic advice.

#### PR comment & wrap-up

**If `--no-git`**: There is no branch or PR to report against. Print the timing table and optimization suggestions to the terminal only. List the files modified in the working tree (`git status --porcelain`) so the user can see what the agent touched. Remind the user that changes are uncommitted — the commit/push/PR steps are theirs to run.

**If `forge != github`**: Report the branch name (and remote URL, if available via `git remote get-url origin`) instead of a PR URL. Print the timing table and optimization suggestions to the terminal only — do **not** attempt to post a PR comment. (Bitbucket `bkt pr comment` wiring is tracked in #10.)

**If `forge == github`**: Report the PR URL. Then post the final step status table as a **PR comment** using `gh pr comment`. Use the markdown table and slowest-step line emitted by `scripts/steps/done` verbatim (strip the trailing `<<<FACTS ... FACTS` block — that's internal). Format:

```
gh pr comment --body "$(cat <<'COMMENT'
## [`/do`](https://github.com/srid/agency) results

| Step | Status | Duration | Verification |
|------|--------|----------|-------------|
| sync | ✓ | 3s | ... |
| research | ✓ | 45s | ... |
...
| **Total** | | **4m 32s** | |

### Optimization suggestions

- <2–4 concrete suggestions based on timing data>

Workflow completed at <timestamp>.
COMMENT
)"
```

---

## Entry Points

| ID               | Starts at             | Use case                                |
| ---------------- | --------------------- | --------------------------------------- |
| `default`        | **sync**              | Full workflow from scratch              |
| `followup`       | **implement**         | Additional changes on existing PR       |
| `post-implement` | **fmt**               | Skip research/impl, start at formatting |
| `polish`         | **hickey+lowy**       | Structural review + quality gate        |
| `ci-only`        | **ci**                | Just run CI                             |

## Rules

- **Never skip steps** (unless skipped by `--no-git` or forge detection). Run them in order from entry point to **done**.
- **Every commit is NEW.** Never amend, rebase, or force-push.
- **Feature branches only.** Never commit to master/main. (Under `--no-git`, no commits happen at all, so this rule is moot — the agent leaves the user on whatever branch they started on.)
- **Background for CI.** Run CI with `run_in_background: true`.
- **No questions.** Don't use `AskUserQuestion` outside the `--review` plan pause (post-research).
- **Never stop between steps.** After completing a step, immediately proceed to the next one.
- **Complete the full workflow.** Implementing code is one step of many. The task is not done until a PR URL (GitHub), a pushed branch name (non-GitHub forges), or a working-tree summary (`--no-git`) is reported.
- **Exhausted retries = halt.** If `ci` or `test` retries are exhausted, set status to `"failed"` and skip to **done**. On `ci` failure the draft PR (opened in the preceding **create-pr** step) stays open as the record of the failed attempt — do not close, undraft, or otherwise mutate it.

ARGUMENTS: $ARGUMENTS
