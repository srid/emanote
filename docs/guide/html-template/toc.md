---
slug: toc
---

# Table of contents

In [[html-template|the default template]], the table of contents lives in the right-panel column at `lg:` (≥1024px) breakpoint and above, alongside [[backlinks]] and the timeline heatmap. Below that breakpoint the right-panel hides entirely and the TOC is omitted to keep the prose centered.

A TOC is rendered only when a page has more than one heading.

## Visual treatment

TOC entries map directly to headings in the prose, which render in plain text — so the TOC stays in a **neutral gray palette** rather than the primary chip language. The currently-visible heading (tracked via `IntersectionObserver` in `_emanote-static/js/toc-spy.js`) is highlighted with a subtle gray background and bold weight. Reserving the primary palette for "this links to another note" affordances keeps the visual semantics tight: primary = navigation between notes, gray = navigation within a note.

## Disabling the ToC

In [[yaml-config]],

```yaml
template:
  toc:
    enable: false
```
