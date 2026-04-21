---
slug: footnote-embed
---

# Footnote embed

Outer note has its own footnote[^outer] and embeds another note that
also has a footnote. Issue #360 was: both rendered with `id="fn1"` /
`id="fnref1"`, producing duplicate IDs on a single page.

[^outer]: Outer note's footnote text.

![[footnote-inner]]
