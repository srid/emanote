---
slug: backlinks
---

# Backlinks

In [[html-template|the default template]], backlinks split into two flavors based on whether the linking note's filename matches an `YYYY-MM-DD[-…]` daily-note pattern:

- **Regular backlinks** — non-daily notes that reference the current page.
- **Timeline backlinks** — daily notes (filenames like `2025-03-14.md`) that reference the current page.

The split is computed in `Calendar.parseRouteDay` and exposed to templates as the `<ema:note:backlinks:nodaily>` / `<ema:note:backlinks:daily>` Heist splices respectively.

## Regular backlinks

Render as a slim labelled list of "tiny title chips" — same chip language as wiki-links — in the right-panel column at `lg:` and above (`#backlinks-margin`), or stacked at the bottom of the card at narrower widths (`#backlinks-bottom`).

Each chip is just the linking note's title; **hovering or focusing** a chip opens a flyout to the left over the prose column with the rendered context paragraphs (the prose around where this note is referenced). The flyout shares typography with the timeline heatmap's cell-hover popup, so all "side material" reads as one family.

On `<lg` (when the right-panel is hidden), the bottom strip carries the same data. Touch devices skip the flyout entirely and see the contexts inlined under the chip.

## Timeline backlinks

Render as a **year-stacked heatmap**: 12 rows per year (one per month), each row 31 cells wide. Cells with a daily backlink are filled in the primary palette and clickable; empty cells are gray. Years stack newest-first.

Hovering a filled cell opens a context flyout (header `YYYY-MM-DD — title`, then the rendered backlink context paragraphs). Same flyout chrome as regular-backlink chips. The heatmap renders in two homes:

- Right-panel at `lg:`+ (compact 4×4 cells in the narrow column)
- Bottom strip at `<lg` (cells stretch as horizontal bars to fill the wider row)

Dates come from the linked note's route via `Calendar.parseRouteDay`, the same parser used for the daily/non-daily split. A daily note can therefore have a custom title without disappearing from the heatmap.

See [[daily-notes]] for how the daily/non-daily split feeds these two surfaces.
