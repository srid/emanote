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

## Running

This suite runs in GitHub Actions (`.github/workflows/ci.yaml`, job
`e2e-tests`) as a matrix over `{live, static}`. There's deliberately no
`just` recipe — Playwright's downloaded Chromium binary depends on
glibc system libs that aren't on `PATH` in a NixOS or plain Nix
devshell, and `npx playwright install --with-deps` shells out to `sudo
apt-get` which doesn't apply on NixOS either. CI (ubuntu-latest) has
both, so that's where this suite runs.

If you do want to iterate locally on a Debian/Ubuntu-like host:

```sh
cd tests
npm ci
npx playwright install --with-deps chromium   # one-time
EMANOTE_BIN=$(command -v emanote) EMANOTE_MODE=live npm test
```

Set `HEADLESS=false` to watch the browser.

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

## Adding scenarios

Add scenarios to `features/smoke.feature` (or a new `.feature` file) —
they run in both modes by default. Only tag scenarios mode-specific
(e.g. `@live-only` for websocket hot-reload) when the behavior is
genuinely backend-bound; the dual-mode run is the whole point.
