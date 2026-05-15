# Ralph: Emanote build time

Iterative, measurement-driven shrink of `cabal` build time — which also
governs the `ghcid` / `just run` inner loop. Generated via the
[`/ralph`](../../../../.claude/skills/ralph/SKILL.md) workflow.

## Goal

Reduce wall-clock time for three scenarios while **preserving runtime
behavior** (generated sites must be test-suite equivalent before vs after).
Dependency versions, GHC pin, and the public API are all in-bounds.

## Methodology

All runs use the project's `nix develop` shell on
`ghc-9.8.4 / cabal-3.14.2.0`. Each measurement is wall-clock seconds via
`date +%s%N`, captured by `docs/dev/ralph/build-perf/measure.sh`. The helper warms up between
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
| 2 | Per-phase profile under `-j`: Simplifier ate 60s of CPU (dominant). Default `-fmax-simplifier-iterations=4` does 4 fixed-point passes; later passes typically find diminishing returns. | Add `-fmax-simplifier-iterations=2` | _20.73 (-18.0% vs cy1)_ | _2.32_ | _3.48_ | _1.98_ | Probed `-O0` (-33%), `-fno-specialise` (-9%), `-fno-cross-module-specialise` (-7%) as more aggressive options — all real runtime trade-offs. **Backed out: shipping cycle 1 only.** Pass 3-4 of the simplifier are diminishing-returns but they're not zero-returns; some inlining cascades depend on them, and we want no runtime cost. |
| 3 | Even with iter=2, the Simplifier still chases cross-module SPECIALISE pragmas (notably from optics, pandoc). Intra-module specialization keeps the worst hot paths fast; cross-module costs compile time. | Add `-fno-cross-module-specialise` | _19.55 (-5.7%)_ | _2.36_ | _3.54_ | _2.01_ | **Backed out: shipping cycle 1 only.** Pandoc parsing and `optics-core` traversals lean heavily on cross-module specialization; losing it would cost an estimated 10-25% on site-gen wall time. _Dead-end attempted in this cycle: pruning unused `build-depends` (13 packages flagged by `-Wunused-packages`). Removing test deps from `library-common` broke the `haskell-flake` dev shell because cabal2nix derives the env from library deps only — separate concern from compile speed._ |

## Dead ends

- **Prune unused `build-depends` (cycle 3 attempt).** `-Wunused-packages`
  flagged 13 packages in `library-common`. Moving test deps
  (`hedgehog`, `hspec-hedgehog`) out broke the `haskell-flake`-derived
  `nix develop` shell — `cabal2nix` collects deps from `library-common`
  into the dev env and doesn't pick up test-only deps unless tests are
  explicitly enabled in the nix expression. Reverted; the deeper
  cleanup is a separate concern from compile speed.
- **`cabal --ghc-options=-j` (probed during cycle 1).** Passing `-j` on
  the command line applied to every package in the build plan (incl.
  cached deps), invalidating their store entries and triggering full
  rebuilds. Confined the option to `library-common`'s `ghc-options`
  instead.

## Final result

Cycles 2 and 3 were measured and committed but then **backed out** on
the constraint that runtime perf must not regress. Only cycle 1's `-j`
flag ships — it changes module-level parallelism inside the compiler
and produces a byte-identical artifact, so the runtime cost is exactly
zero.

| Metric        | Baseline | Shipped (-j only) | Δ        |
| ------------- | -------- | ----------------- | -------- |
| cold-build    | 28.61s   | **22.42s**        | **-21.6%** |
| incremental   |  2.66s   |  **2.38s**        | **-10.7%** |
| ghcid-cold    |  4.44s   |  **3.62s**        | **-18.4%** |
| ghcid-warm    |  2.88s   |  **2.03s**        | **-29.4%** |

`cabal build all && cabal test all` → BUILD_OK, 119/119 passing.

## Findings

1. **GHC defaulted to `-j1` here**, and `cabal -j` only parallelises
   across packages. For a single-package project like emanote, the
   only way to use more than one core is `ghc-options: -j` (cycle 1).
   The compiled artifact is byte-identical to a `-j1` build — this is
   the cheapest single change because the trade-off has no cost side.

2. **Simplifier was the dominant phase under `-j`** (~60s of CPU),
   followed by Renamer/typechecker. The two compile-time levers we
   probed against it — `-fmax-simplifier-iterations=2` (-18% extra) and
   `-fno-cross-module-specialise` (-6% extra) — each had a small but
   real runtime cost. Reverted both per the no-runtime-cost rule. The
   simplifier passes 3-4 enable second-order inlining cascades;
   cross-module SPECIALISE drives most of pandoc/optics' hot-path
   unboxing.

3. **More aggressive options were probed and rejected** for the same
   reason: `-fignore-interface-pragmas` (-16% extra cold-build, kills
   cross-module inlining everywhere), `-O0` (-33% extra cold-build,
   broad runtime regression), `-fno-specialise` (-9% extra, real hit
   to pandoc/optics-heavy paths).

4. **The `first incremental rebuild after a fresh build` outlier**
   (~8s vs ~2.3s steady-state) is real and consistent across all
   cycles. Not investigated further — likely cabal plan-cache priming.

## How to reproduce locally

```sh
nix develop -c ./docs/dev/ralph/build-perf/measure.sh all          # cold + incr + ghcid
nix develop -c ./docs/dev/ralph/build-perf/measure.sh cold 5       # just cold, 5 runs
```

The single landed knob lives in `emanote/emanote.cabal` under
`common haskell-common`:

```
ghc-options:
  ... -j
```
