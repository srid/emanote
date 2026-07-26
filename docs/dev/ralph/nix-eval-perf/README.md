# Ralph: Nix command latency

Iterative, measurement-driven reduction of the time developers wait for
Emanote's Nix developer shell and app. The shipped change follows the same
zero-input-flake design used by Kolu: the user-facing flake contains no flake
inputs, while npins supplies the same pinned revisions to standalone Nix
libraries.

## Goal

Reduce the warm latency of `nix develop` and `nix run` without changing the
Emanote package, app, docs, checks, or public flake modules. Cold evaluation is
measured as a guardrail, but the primary metric is the repeated command latency
felt during normal development.

## Methodology

Measurements were taken on `x86_64-linux` with Nix 2.34.7. Each result is the
median of five wall-clock runs. The baseline is `master` at
`9d0103737216e5f36f3642eadb31e39dcb9c95f1`; the final measurements use the
staged working tree.

| Mode | Method |
| --- | --- |
| warm | Run the command once to populate Nix's evaluation cache, then measure five invocations |
| no eval cache | Add `--no-eval-cache` to each invocation |
| fresh client cache | Give each invocation a new `XDG_CACHE_HOME`; the Nix store remains populated |

The measured commands are:

```sh
nix develop --quiet -c true
nix run --quiet . -- --help
```

Fresh-client-cache runs include source resolution and may contact remotes. They
are not empty-store builds.

## Before and after

| Command | Mode | Baseline samples (s) | Final samples (s) | Baseline median | Final median | Delta |
| --- | --- | --- | --- | ---: | ---: | ---: |
| `nix develop` | warm | 0.28, 0.28, 0.28, 0.28, 0.29 | 0.22, 0.22, 0.23, 0.23, 0.23 | 0.28s | **0.23s** | **-17.9%** |
| `nix develop` | no eval cache | 2.67, 2.76, 2.77, 2.78, 3.13 | 2.52, 2.53, 2.58, 2.58, 2.62 | 2.77s | **2.58s** | **-6.9%** |
| `nix develop` | fresh client cache | 2.98, 3.06, 3.08, 3.22, 3.25 | 2.80, 2.83, 2.87, 2.87, 3.06 | 3.08s | **2.87s** | **-6.8%** |
| `nix run` | warm | 0.14, 0.15, 0.15, 0.15, 0.16 | 0.10, 0.11, 0.11, 0.11, 0.11 | 0.15s | **0.11s** | **-26.7%** |
| `nix run` | no eval cache | 1.63, 1.64, 1.68, 1.68, 1.70 | 1.76, 1.76, 1.78, 1.79, 1.82 | 1.68s | 1.78s | +6.0% |
| `nix run` | fresh client cache | 1.94, 1.96, 1.97, 1.99, 2.03 | 1.98, 1.99, 1.99, 2.00, 2.04 | 1.97s | 1.99s | +1.0% |

The warm path improves substantially for both target commands. Developer-shell
cold evaluation also improves. The standalone `haskell-flake` path costs about
100 ms more when forcing app evaluation without Nix's evaluation cache; this is
retained because the normal app path is 40 ms faster and fresh-client-cache
latency is effectively unchanged.

## Shipped design

1. The root `flake.nix` has no inputs. `flake.lock` is replaced by npins while
   retaining every dependency revision.
2. `haskell-flake` is evaluated through its documented standalone
   `nix/lib.nix` API, without flake-parts or nixos-unified.
3. Runtime outputs disable `haskell-flake`'s development shell. The full
   Haskell shell remains available to `nix develop`.
4. Formatting hooks move to `nix develop .#fmt`, so ordinary shell entry does
   not evaluate git-hooks and fourmolu. `just fmt` selects that shell.
5. The diagram, Playwright, and Chrome DevTools shells consume the npins sources
   directly. The public Emanote-site flake-parts module remains an output for
   downstream users; it is not involved in evaluating this repository's root
   flake.

Update all pins with `npins update`, or update one pin with, for example,
`npins update nixpkgs-latest`.

## Optimization log

| Cycle | Mutation | Result | Decision |
| --- | --- | --- | --- |
| 1 | Put an npins compatibility layer under the existing nixos-unified/flake-parts graph | warm develop 0.34s; warm run 0.24s | Rejected: retained the expensive module graph and regressed both targets |
| 2 | Use the standalone `haskell-flake` library | warm develop 0.29s; warm run 0.14s | Retained as the simpler foundation |
| 3 | Load npins sources through nixpkgs fetchers | no-cache develop 3.07s; run 1.79s | Rejected: no improvement over the standard npins loader |
| 4 | Replace git-hooks evaluation with a checked-in pre-commit config and static packages | warm develop 0.41s; no-cache develop 3.16s | Rejected: regressed shell entry |
| 5 | Generate and check in `emanote/cabal.nix` | no-cache develop 3.32s; run 1.79s | Rejected: no improvement |
| 6 | Move formatting hooks to `.#fmt` | warm develop 0.22-0.23s | Retained: 18-21% shell improvement |
| 7 | Disable the Haskell dev shell for runtime-only outputs | warm run 0.10-0.11s | Retained: 27-33% app improvement |

Three consecutive speculative changes failed the 3% improvement threshold, so
the loop stopped after isolating the two output-specific wins.

## Compatibility checks

- Output names remain `packages.{default,emanote,docs}`,
  `apps.{default,emanote,docs}`, `devShells.{default,diagrams}`, and the existing
  checks. `devShells.fmt` is additive.
- The evaluated `apps.x86_64-linux.default.program` store path is unchanged from
  the baseline.
- `homeManagerModule`, the downstream `flakeModule`, the template, and the
  Omnix CI contract remain exported.
- The pin migration preserves the revisions previously recorded in
  `flake.lock`.

## Reproduce locally

Warm each command once, then time five runs:

```sh
nix develop --quiet -c true
for _ in 1 2 3 4 5; do time nix develop --quiet -c true; done

nix run --quiet . -- --help
for _ in 1 2 3 4 5; do time nix run --quiet . -- --help; done
```

Use `--no-eval-cache` for forced evaluation. For the fresh-client-cache metric,
set `XDG_CACHE_HOME` to a newly created temporary directory for each run.
