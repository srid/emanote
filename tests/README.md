# Emanote e2e tests

Cucumber + Playwright smoke tests that exercise the live server
(`emanote run`) and the static build (`emanote gen`) against the same
backend-agnostic scenarios. Any difference between the two is by
definition a regression — the #633 Tailwind v4 migration surfaced a
handful of "works in dev, broken in prod" defects that the Haskell
unit tests and `htmlproofer` link check couldn't see. This suite is
the guard against repeats.

See [issue #634](https://github.com/srid/emanote/issues/634) for the
full motivation.

## Running locally

```sh
just e2e-live    # spawns `emanote run`, runs the suite against it
just e2e-static  # `emanote gen` once, then serves the output
```

Runtime provisioning (Node + Playwright-compatible Chromium) lives in
`tests/shell.nix`. The npm step downloads the cucumber toolchain on
first run; subsequent runs reuse `node_modules`.

Set `HEADLESS=false` to watch the browser.

## CI

GitHub Actions (`.github/workflows/ci.yaml`, job `e2e-tests`) runs the
matrix `{live, static}` directly with apt-provided Chromium, which is
cheaper on Ubuntu runners than spinning a Nix devshell. CI is the
single source of truth for which modes are tested.

## Layout

- `features/` — Gherkin, backend-agnostic. The same `.feature` files
  run in both modes.
- `step_definitions/` — Playwright + `@cucumber/cucumber`. Steps never
  branch on mode; `baseUrl` is the only thing that changes.
- `support/hooks.ts` — owns the `EMANOTE_MODE` volatility axis:
  spawns `emanote run` or generates + serves static output.
- `support/world.ts` — per-scenario state (browser page, helpers).
- `fixtures/notebook/` — minimal notebook consumed by both modes. Not
  coupled to `docs/` so doc edits never flake tests.
- `shell.nix` — Nix shell providing Node + Playwright-compatible
  Chromium for local runs.

## Adding scenarios

Add scenarios to `features/smoke.feature` (or a new `.feature` file) —
they run in both modes by default. Only tag scenarios mode-specific
(e.g. `@live-only` for websocket hot-reload) when the behavior is
genuinely backend-bound; the dual-mode run is the whole point.
