# Ralph: Emanote build time

Iterative, measurement-driven shrink of `cabal` build time — which also
governs the `ghcid` / `just run` inner loop. Generated via the
[`/ralph`](.claude/skills/ralph/SKILL.md) workflow.

## Goal

Reduce wall-clock time for three scenarios while **preserving runtime
behavior** (generated sites must be test-suite equivalent before vs after).
Dependency versions, GHC pin, and the public API are all in-bounds.

## Methodology

All runs use the project's `nix develop` shell on
`ghc-9.8.4 / cabal-3.14.2.0`. Each measurement is wall-clock seconds via
`date +%s%N`, captured by `.ralph/measure.sh`. The helper warms up between
runs as appropriate and reports per-run + median.

| Metric                  | What it runs                                                     |
| ----------------------- | ---------------------------------------------------------------- |
| **cold-build**          | `cabal clean && cabal build all -v0` (no ghcid flag)             |
| **incremental**         | warm cache, append a `-- ralph-bump` line to a leaf module, then `cabal build all -v0` |
| **ghcid-cold**          | `cabal clean && echo :quit \| cabal repl exe:emanote --flags=ghcid` |
| **ghcid-warm**          | same as above without `cabal clean` (typical relaunch)           |

The leaf for incremental is `emanote/src/Emanote/View/TaskIndex.hs` (no
intra-`src` reverse deps; downstream is only the test-suite).

Runs use the project's library + executable + test-suite — `cabal build
all` exercises the realistic CI path; `cabal repl --flags=ghcid` exercises
the ghcid path (where `src/` is inlined into the exe and the library is
skipped). Dependency-package compiles are excluded — they live in
`~/.cabal/store` and are stable across runs.

## Baseline (origin/master, commit
[d383051a](https://github.com/srid/emanote/commit/d383051a))

| Metric        | Runs (s)                            | Median (s) |
| ------------- | ----------------------------------- | ---------- |
| cold-build    | 28.847, 28.607, 28.563              | **28.61**  |
| incremental   | 8.944, 2.659, 2.642, 2.633, 2.677   | **2.66**   |
| ghcid-cold    | 4.418, 4.492, 4.433                 | **4.44**   |
| ghcid-warm    | 2.878, 2.901, 2.846                 | **2.88**   |

> The first incremental run after a freshly warmed `cabal build all`
> consistently inflates (cabal plan / cache priming) — subsequent runs are
> the steady-state inner loop the developer feels.

## Optimization log

Each row records: cycle number, hypothesis (what we thought was the biggest
contributor), the change made, and the median before/after for each
metric. Changes that don't beat noise (≥3% improvement) get a row but
**aren't committed**.

| # | Hypothesis | Mutation | cold (s) | incr (s) | ghcid-cold (s) | ghcid-warm (s) | Notes |
| - | ---------- | -------- | -------- | -------- | -------------- | -------------- | ----- |
| _baseline_ | — | — | 28.61 | 2.66 | 4.44 | 2.88 | reference |
| 1 | GHC defaulted to `-j1`; `cabal -j` only parallelises across packages (we have one). Module-level parallelism was the cheapest leverage. | `ghc-options: -j` in `library-common` | **25.30** (-11.6%) | **2.41** (-9.4%) | **3.62** (-18.5%) | **2.04** (-29.2%) | `-j4` slightly faster than `-j` (21.5s vs 22.1s) but `-j` adapts to host cores. Probed `-j1/-j2/-j4/-j8/-j` first to confirm scaling stops at ~4. |
| 2 | Per-phase profile under `-j`: Simplifier ate 60s of CPU (dominant). Default `-fmax-simplifier-iterations=4` does 4 fixed-point passes; later passes typically find diminishing returns. | Add `-fmax-simplifier-iterations=2` | **20.73** (-18.0% vs cy1) | **2.32** (-3.7%) | **3.48** (-3.9%) | **1.98** (-3.0%) | Probed `-O0` (-33%), `-fno-specialise` (-9%), `-fno-cross-module-specialise` (-7%) as more aggressive options — all real runtime trade-offs. Iter=2 is the lightest perf tax with the biggest cold-build payoff. `cabal test all` still passes 119/119. |

## Dead ends

_Filled in as the loop progresses — “tried X, no measurable change, here's
why” lives here, so future Ralph runs don't repeat the experiment._

## Findings

_TBD. Will summarize the structural lessons (which modules dominate, which
extensions / deps matter, etc.) at the end of the run._

## How to reproduce locally

```sh
nix develop -c ./.ralph/measure.sh all          # cold + incr + ghcid
nix develop -c ./.ralph/measure.sh cold 5       # just cold, 5 runs
```
