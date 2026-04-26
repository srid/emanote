# /do config

## Check command
cabal build all

## Format command
just fmt

## Test command
- `just test` — Haskell unit tests
- `just e2e-live` / `just e2e-static` — cucumber+Playwright e2e (live and static modes)

## CI command
vira ci

## Documentation
Keep `README.md`, `docs/` (user documentation), and `CHANGELOG.md` (under the `Unreleased` section) in sync with user-facing changes.

<!-- Optional (add manually for the evidence step):
## PR evidence
-->
