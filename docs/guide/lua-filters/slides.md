---
slug: lua-filters/slides
short-title: Slides demo
pandoc:
  filters:
    render:
      html:
        - filters/slides.lua
---

# A Markdown Presentation about Lua Filters

For setup, phase guidance, and bundled filters, see [[lua-filters|the main Lua filters guide]].

This page is itself a demo of two things at once: a custom **`slides.lua`** filter that turns a `::: slides` div into a navigable deck, *and* a tour of what Pandoc Lua filters look like inside [[markdown|Markdown]] notes. Use the numbered nav above the deck (or the <kbd>←</kbd> / <kbd>→</kbd> keys, after clicking inside the deck) to step through.

::: slides

## Why filters?

Pandoc parses your [[markdown|Markdown]] into a typed AST — paragraphs, headings, [[wikilinks|links]], code blocks. **A Lua filter is a function that walks that tree and rewrites it** before Emanote renders to [[html-template|HTML]].

Anything you can say about a node in Lua, you can transform.

## Adding one to your notebook

1. Drop a `.lua` file anywhere in your [[layer|notebook layer]] (convention: a `filters/` folder).
2. Reference it from the note's [[yaml-config|frontmatter]]:

```yaml
pandoc:
  filters:
    render:
      html:
        - filters/slides.lua
```

3. Save. Emanote resolves the path against every `-L`'d [[layer]].

## What this page does

This page declares `pandoc.filters.render.html: [filters/slides.lua]`. Each `## ` heading inside a `::: slides` div becomes one slide; the filter wraps them in `<section>` elements, prepends a nav strip, and emits [[custom-style|CSS]] + a tiny arrow-key handler.

_See the source: [`docs/filters/slides.lua`](https://github.com/srid/emanote/blob/master/docs/filters/slides.lua)._

## Hot-reload

Edit `filters/slides.lua` — change a colour, tweak the layout, add a feature — and **every note that references it refreshes on the next save**. No `touch` of the `.md` required, no live-server restart.

The reverse index from filter path → dependent notes is maintained inside Emanote's model; an edit invalidates exactly the affected notes, and the render-time filter runs again with `FORMAT == "html"`.

## A second example: word count

Pure-Lua filters compose cleanly. The [[lua-filters|main guide page]] chains `list-table.lua` with `wordcount.lua`:

```yaml
pandoc:
  filters:
    parse:
      - lua-filters/list-table.lua
      - lua-filters/wordcount.lua
```

Both are bundled in Emanote's [[layer|default layer]] and run on every save; the order matches the array.

## Writer-specific filters

Many Pandoc Lua filters branch on Pandoc's `FORMAT` variable to emit [[html-template|HTML]] or LaTeX. Put [[markdown|Markdown]]-agnostic AST rewrites under `pandoc.filters.parse`, and put HTML-specific filters under `pandoc.filters.render.html`.

This deck uses the render-time slot because `slides.lua` emits raw [[markdown|HTML]], [[custom-style|CSS]], and JavaScript.

## Where to find more

- [pandoc-ext/list-table](https://github.com/pandoc-ext/list-table) — the maintained upstream for the bundled list-table filter
- [pandoc-ext/info](https://github.com/pandoc-ext/info) — a catalog of Pandoc extensions
- [Pandoc Lua filter reference](https://pandoc.org/lua-filters.html) — the API
- [[lua-filters]] — Emanote's main guide

The pattern is always the same: drop the `.lua` in, name it in [[yaml-config|frontmatter]], save.

:::

This deck is rendered by `slides.lua`. If you view source, you'll see _it_ is plain [[markdown|Markdown]] inside one fenced div — the filter does the [[html-template|HTML-specific]] structural work at render time.
