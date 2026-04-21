---
argument-hint: <github-issue-url | prompt> [--review] [--from <step>]
description: Do a task end-to-end — implement, PR, CI loop, ship
---

# Do Workflow

Take a task and do it top-to-bottom: research, implement, open a draft PR, pass CI, refine, and ship.

**Fully autonomous.** Do NOT use `AskUserQuestion` at any point (unless `--review` is active during the planning pause). Make sensible default choices and keep moving.

## Arguments

Parse the arguments string: `[--review] [--from <step-id>] <task description or github-issue-url>`

- `--review`: Pause after **hickey** for user plan approval via `EnterPlanMode`/`ExitPlanMode`, then continue autonomously
- `--from <step-id>`: Start from a specific step (see entry points below)

## Results Tracking

After each step's verification, write/update `.do-results.json`:

```json
{
  "workflow": "do",
  "startedAt": "<ISO timestamp>",
  "active": true,
  "status": "running",
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

- Set `active` to `true` when the workflow starts (**sync**), and `false` when it ends (**done**). The stop hook uses this field to block premature exits.
- Set `status` to `"completed"` when **done** is reached, or `"failed"` if halted. This field is informational only.
- Use the Write tool to update the file after each step.
- Capture timestamps via Bash: `date -u +%Y-%m-%dT%H:%M:%SZ`. Do not guess or hallucinate timestamps.

## Steps

Print a progress line before each step:

```
[do] ✓sync ✓research ▸hickey · branch · implement · docs · police · fmt · commit · test · ci · update-pr · done
```

### sync

Run: `git fetch origin && git remote set-head origin --auto`

If current branch is behind origin, fast-forward with `git pull --ff-only`.

**Verify**: git fetch ran without error.

---

### research

Research the task thoroughly before writing code.

- If given a GitHub issue URL, fetch with `gh issue view`
- Use Explore subagents, Grep, Glob, Read — whatever it takes to understand the problem
- **Never assume** how something works. Read the code. Check the config.
- If the prompt involves external tools/libraries, use WebSearch/WebFetch

**Verify**: Can articulate what needs to change, where, and why.

---

### hickey

Evaluate the planned approach for structural simplicity. Invoke the `hickey` skill via the Skill tool.

- Identify concerns. Check for complecting. Suggest simplifications.
- Revise the approach to eliminate accidental complexity before proceeding.

**If `--review`**: After hickey completes, use `EnterPlanMode` to present the revised approach for user approval:

- **Clarify ambiguities** first — ask via `AskUserQuestion` if anything is unclear. Don't guess.
- **High-level plan**: what to do and why, not implementation details. Include an **Architecture section** (affected modules, new abstractions, ripple effects).
- **Split non-trivial plans into phases** — MVP first, each phase functionally self-sufficient.
- Include a **Simplicity assessment** noting what hickey found and any trade-offs accepted.

Use `ExitPlanMode` to present the plan. Once approved, continue autonomously from **branch**.

**Verify**: Complecting concerns addressed or justified.

---

### branch

Detect the default branch: `git symbolic-ref refs/remotes/origin/HEAD`

1. Create a descriptive feature branch from `origin/<default>`
2. Create an empty commit: `git commit --allow-empty -m "chore: open PR"`
3. Push the branch
4. Open a draft PR: `gh pr create --draft`

**MANDATORY**: Load the `github-pr` skill (via Skill tool) BEFORE writing the PR title/body.

5. **Post hickey results**: If the hickey step produced findings with suggestions, post the full hickey analysis as a PR comment using `gh pr comment`. Use a `## Hickey Analysis` header. Skip this if hickey found no issues.

**Verify**: On a feature branch (not master/main), draft PR exists (`gh pr view` succeeds). If hickey had findings, a PR comment exists.

---

### implement

If the task is a bug fix: write a failing test first (e2e or unit, whichever is appropriate), then fix the bug.

Otherwise: implement the planned changes. Prefer simplicity. Do the boring obvious thing.

**E2E coverage**: When the change introduces multiple user-facing paths (e.g., a dialog that appears under different conditions), write e2e scenarios for **each distinct path**. Enumerate the user-visible paths, then check that every one has a corresponding test.

**Verify**: Code changes match the planned approach. All distinct user-facing paths have test coverage.

---

### docs

Read the project's instructions to find which documentation files to keep in sync (e.g., README.md). Compare those files against changes in this PR.

If no documentation files are documented, skip this step with a note.

**Verify**: Docs match current code.
**If outdated** (max 3 attempts): Fix the outdated sections and re-verify.

---

### police

Use `git diff <default-branch>...HEAD --name-only` to check if the PR contains code changes. If all changed files are documentation-only (e.g., `.md`, `.txt`, `README`, docs/) — skip this step with a note.

Otherwise, invoke the `/code-police` skill via the Skill tool. It runs three passes: rule checklist, fact-check, and elegance.

When `/code-police` asks about scope: **changes in the current branch/PR only**.

**Verify**: All 3 passes clean ("All clear").
**If violations found** (max 3 attempts): Fix the violations and re-invoke `/code-police`.

---

### fmt

Read the project's instructions to find the format command (typically documented in a workflow instruction). Run it.

If no format command is documented, skip this step with a note.

**Verify**: Format command ran without error, or no command configured.

---

### commit

Create a NEW commit (never amend) with a conventional commit message. Push to the PR branch.

**Verify**: `git log -1` shows a new commit on the feature branch, and it's pushed to remote.

---

### test

Read the project's instructions to find the test command and strategy. Run only the tests relevant to the code paths changed in this PR.

Use `git diff <default-branch>...HEAD --name-only` to identify changed files and determine which tests are relevant.

If changes are purely internal with no user-facing impact, unit tests may suffice — skip e2e if no relevant scenarios exist. If no test command is documented, skip with a note.

**Verify**: Tests pass (exit code 0), or no relevant tests to run.
**If failed** (max 4 attempts): Analyze the failure. If flaky, re-run. If real: fix → go to **fmt**, then retry.

---

### ci

Read the project's instructions to find the CI command and verification method. Run CI with `run_in_background: true` if the command takes more than a few seconds.

**Never pipe CI to `tail`/`head`**, and **never append `2>&1`** — background mode captures both streams.

**Verify**: Use the verification method described in the project's instructions (e.g., checking commit statuses, reading CI output). If no CI command is documented, skip with a note.

**On failure** — read logs or output to diagnose.

**Flaky vs real**: A test is flaky only if it **passes on a subsequent retry**. Consistent failure = real bug. Before retrying, read the failing test code to judge if the failure pattern is inherently flaky (race conditions, timing, async waits).

**If flaky** (max 3 retries): Retry just the failing step.
**If real bug** (max 5 fixes): Fix → **fmt** → **commit** → retry CI.
**If retries exhausted**: Set workflow status to `"failed"`, skip to **done**.

---

### update-pr

Re-check the PR title/body against current scope. If scope changed, update via `gh pr edit` per the `github-pr` skill.

**Verify**: PR title/body matches the delivered scope.

---

### done

Present a summary of all steps with their verification status. If any step has a non-success status, retry it (max 3 attempts from done). If still failing after retries, set `status: "failed"`.

`"completed"` requires **all steps passed**. No redefining "passed," no footnote caveats. Update `.do-results.json` accordingly.

#### Timing summary

Compute duration for each step from its `startedAt`/`completedAt` timestamps. Print a table to the user showing each step's duration and the total wall-clock time (`startedAt` of first step → `completedAt` of last step). Highlight the **slowest step** and any step that took >30% of total time.

#### Optimization suggestions

After the timing table, print 2–4 concrete suggestions for reducing time-to-completion in future runs. Base these on the actual timing data — for example:

- If **ci** dominates: suggest `--from ci-only` for re-runs, or note which CI sub-step was slowest
- If **research** was slow: suggest pre-reading relevant code before invoking `/do`
- If **test** had retries: note the flaky test and suggest hardening it
- If **police** required fix iterations: note which pass caught issues (rules/fact-check/elegance)
- If **implement** was the bottleneck: suggest breaking the task into smaller PRs

Be specific to this run's data, not generic advice.

#### PR comment & wrap-up

Report the PR URL. Then post the final step status table as a **PR comment** using `gh pr comment` with a markdown table including durations. Format:

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

| ID               | Starts at     | Use case                                |
| ---------------- | ------------- | --------------------------------------- |
| `default`        | **sync**      | Full workflow from scratch              |
| `followup`       | **implement** | Additional changes on existing PR       |
| `post-implement` | **fmt**       | Skip research/impl, start at formatting |
| `polish`         | **police**    | Just the quality gate                   |
| `ci-only`        | **ci**        | Just run CI                             |

## Rules

- **Never skip steps.** Run them in order from entry point to **done**.
- **Every commit is NEW.** Never amend, rebase, or force-push.
- **Feature branches only.** Never commit to master/main.
- **Background for CI.** Run CI with `run_in_background: true`.
- **No questions.** Don't use `AskUserQuestion` unless `--review` is active during the hickey pause.
- **Never stop between steps.** After completing a step, immediately proceed to the next one.
- **Complete the full workflow.** Implementing code is one step of many. The task is not done until a PR URL is reported.
- **Exhausted retries = halt.** If `ci` or `test` retries are exhausted, set status to `"failed"` and skip to **done**. Do not proceed to `update-pr` as if nothing happened.

ARGUMENTS: $ARGUMENTS