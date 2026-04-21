# E2E Fixture

Home page for e2e tests. Site theme is `blue` (see `index.yaml`).
See [themed](themed.md) for a per-page `theme: green` override.

The bracketed-span below is a live-mode Tailwind palette probe. The CDN
tree-shakes `--color-<palette>-*` unless it sees a matching class
attribute on an element. Without this probe, `--color-blue-500` would be
undefined and `--color-primary-500: var(--color-blue-500)` would resolve
to empty, so tests would fail on a minimal fixture instead of on a real
regression. Static mode doesn't need the probe (the CLI scans `<style>`
blocks too).

[palette probe]{.sr-only .text-blue-500 .text-green-500}
