---
slug: right-panel
---

# Right panel

In [[html-template|the default template]], the right-panel is the column attached to the right edge of the main card at `lg:` (≥1024px) and above. It mirrors the [[sidebar]]'s chrome (gray background, sticky-scrolling, matching width tiers — `lg:w-52` / `xl:w-72`) so the two read as a symmetric pair.

The panel hosts three kinds of "side material", stacked top-to-bottom:

1. [[toc|Table of contents]] — neutral-gray hierarchy of the current page's headings, with scroll-spy highlighting the section in view.
2. **Timeline heatmap** — year-stacked grid of cells where filled days are [[backlinks|daily-note backlinks]] to the current page (see [[backlinks#timeline-backlinks]]).
3. **Linked from** — slim chip list of regular [[backlinks|backlinks]] (non-daily notes that reference the current page); each chip opens a context flyout on hover.

Below `lg:`, the right-panel hides and a footer-attached strip at the bottom of the card carries the timeline heatmap + regular backlinks list (the TOC is omitted at narrow widths, since the prose dominates).

## Why this column exists

Pre-#699 the default theme rendered TOC as a side column nested inside the prose body and pushed backlinks below the article as a separate floating card. The right-panel consolidates both into one attached column inside the same `#container` card as the sidebar and prose, so the page reads as a single composed unit instead of stacked stripes.

## Disabling

The TOC and backlinks each have their own enable knobs (see [[toc]] and [[backlinks]]). The right-panel renders only when there's content for at least one of them — pages with no TOC and no backlinks omit it entirely (no empty gray column).
