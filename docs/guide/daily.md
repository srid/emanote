---
slug: daily
---

# Daily Notes

Emanote has special support for [Obsidian style daily notes](https://help.obsidian.md/plugins/daily-notes).

If you create notes named `YYYY-MM-DD.md`, Emanote will treat them as daily notes. This has a couple of effects:

1. The backlinks panel will render daily notes separate from regular notes. The daily notes will be rendered as a "timeline" in reverse chronological order.
2. Each daily note automatically gets a hierarchical tag (eg: `#calendar/2025/03`) allowing you to browse them by calendar navigation in the tag index.

## Timeline backlinks demo

The sample notes below all link back to this page. Notes whose filenames begin
with `YYYY-MM-DD` show up in the Timeline panel at the end of this page, while
the ordinary backlink from the [[obsmd|Obsidian]] note shows up in the regular
"Links to this page" panel.

```query
path:./*
```
