---
name: ralph
description: Iterative measurement-driven improvement loop. Measure, profile, mutate, re-measure, commit. Works for performance, bundle size, complexity, test coverage — anything quantifiable. Use when the user wants to systematically improve a metric through repeated cycles of profiling and targeted changes.
---

# Ralph

Iterative measurement-driven improvement loop. Each cycle: measure → profile → find biggest contributor → mutate → re-measure → commit only if improvement exceeds noise.

## 0. Gather inputs

Use `AskUserQuestion` to collect:

1. **What to improve** — target metric and direction
2. **How to measure** — command or method (research and propose one if user doesn't know)
3. **How many cycles** — default 20
4. **Constraints** — what must NOT change

## 1. Setup

- Create a feature branch and **draft PR** early (load `forge-pr` skill for title/body). PR description includes a measurements table updated as cycles complete.
- **Baseline**: measure at least 5 runs, report **median**. For time: distinguish cold (no cache) from hot (cached). Document methodology.
- Create `docs/<target>-ralph-report.md` with baseline, methodology, optimization log table, and findings.
- Seed `TaskCreate` list with N cycle tasks.

## 2. The loop

Each cycle:

1. **Profile** — break down the metric into components. Measure each independently. Don't guess.
2. **Classify** — categorize the biggest contributor (unnecessary dep, eager eval, redundant work, wrong abstraction, missing cache, structural overhead).
3. **Mutate** — single, targeted change addressing the biggest contributor.
4. **Re-measure** — same benchmark, same methodology. If improvement is within noise (<3% for time), don't commit — document in report only.
5. **Commit + push** — only if improvement exceeds noise. Include metrics in commit message. Push report file with each commit.

## 3. Wrap-up

- **Final measurement** with same methodology as baseline — this is the number for the PR.
- **Update PR description** with final before/after table, summary of changes, key findings.
- **Complete report** with optimization log, dead ends ("Investigated but no improvement"), key findings, and cost breakdown.
- **Run CI** to verify nothing is broken.

## Rules

- **Facts over opinions.** Measure everything. Don't commit based on theory.
- **One change per cycle.** Isolate variables.
- **Only commit improvements.** Noise-level changes clutter history.
- **Preserve behaviour.** All changes behaviour-preserving unless user explicitly allows otherwise.
- **Document dead ends.** "X doesn't help" is valuable knowledge.
- **Stop at diminishing returns.** 3 consecutive no-improvement cycles → tell user and stop.
- **Keep the report.** The `.md` is a deliverable — useful for blog posts and future reference.
