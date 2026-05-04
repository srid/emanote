---
slug: daily-notes
---

# Daily Notes

Emanote has special support for [Obsidian style daily notes](https://help.obsidian.md/plugins/daily-notes).

If you create notes named `YYYY-MM-DD.md`, Emanote will treat them as daily notes. This has a couple of effects:

1. The backlinks panel will render daily notes separate from regular notes. Daily notes render as a year-stacked **timeline heatmap** (see [[backlinks#timeline-backlinks]]); regular notes render as a "Linked from" chip list.
2. Each daily note automatically gets a hierarchical tag (eg: `#calendar/2025/03`) allowing you to browse them by calendar navigation in the tag index.
3. Sidebar tree nodes whose immediate children are all daily notes of the same month (typically a `Daily/2026/04/` folder) render as a small **7-column calendar grid** in place of the linear list — each filled cell links to that day's note, missing days are muted dots.

## Timeline backlinks demo

The sample notes below all link back to this page. Notes whose filenames begin
with `YYYY-MM-DD` show up in the timeline heatmap (in the [[right-panel]] at
`lg:`+ widths, or the bottom strip at narrower widths), while the ordinary
backlink from the [[obsmd|Obsidian]] note shows up in the "Linked from" chip
list alongside it.

```query
path:./*
```
