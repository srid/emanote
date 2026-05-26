---
slug: authoring
order: 2
---

# Authoring

What you type into a note — input formats, link syntax, graph structures, render features that an author reaches for from a Markdown buffer.

- **[[markdown]]** — extensions on top of CommonMark: callouts, task lists, emojis, footnotes, footers
- **[[wikilinks]]** — `[[…]]` syntax, structural links, broken / ambiguous link rendering
- **[[query]]** — Obsidian-style embed queries for dynamic listings
- **[[folgezettel]]** — structural wikilinks that drive [[sidebar]] hierarchy
- **[[daily-notes]]** — date-stamped notes
- **[[orgmode]]** — Org-mode notes alongside Markdown
- **[[math]]**, **[[mermaid]]**, **[[syntax-highlighting]]**, **[[adding-images]]** — built-in render features the author types directly
- **[[feed]]**, **[[export]]** — frontmatter-declared outputs

Cross-cutting: configuration that affects what you can type lives under [[config]] ([[yaml-config]], [[layer]]); output customisation lives under [[theme]]; code-level extension lives under [[extend]].

```query
path:./*
```
