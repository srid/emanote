---
template:
  theme: green
---

# Themed

Per-page `template.theme: green` override. The `#emanote-theme-remap`
`<style>` in the rendered HTML should alias `--color-primary-*` to
`var(--color-green-*)`, and the green palette must be emitted in `:root`
for the alias to resolve — that's the #633-class regression we're
guarding against.
