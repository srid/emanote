---
slug: lua-filters
short-title: Lua Filters
pandoc:
  filters:
    parse:
      - lua-filters/list-table.lua
      - lua-filters/wordcount.lua
---

# Pandoc Lua Filters

A [Pandoc Lua filter](https://pandoc.org/lua-filters.html) rewrites the parsed Pandoc document before Emanote turns it into a page.

Filters are note-local. Enable them in Markdown frontmatter or, for Org notes, with `#+PANDOC_FILTERS:`.

## Choose a phase

Emanote supports two filter phases:

| Phase | YAML key | Runs with | Best for |
| --- | --- | --- | --- |
| Parse time | `pandoc.filters.parse` | `FORMAT == "markdown"` | Cheap, pure AST rewrites that should affect Emanote's model |
| Render time | `pandoc.filters.render.html` | `FORMAT == "html"` | HTML-specific output and IO work |

### Parse-time filters

Parse-time filters run immediately after Markdown parsing:

```yaml
pandoc:
  filters:
    parse:
      - lua-filters/list-table.lua
```

Use parse-time filters when the rewritten document should affect Emanote's semantic model:

- title extraction
- tags and links
- backlinks
- tasks
- table structure
- search text

Parse-time filters should stay cheap. They run when Emanote parses notes, so expensive work here slows model updates even when Emanote only needs metadata, links, search text, or other non-HTML data.

Parse-time filters cannot use IO. Emanote rejects direct references before running the filter and also runs parse filters with IO-capable APIs disabled. That includes:

- Lua APIs such as `io`, `os`, `print`, `require`, `load`, `dofile`, and `debug`
- Pandoc APIs such as `pandoc.pipe`, `pandoc.system`, `pandoc.mediabag`, `pandoc.template`, and `pandoc.zip`
- nested Pandoc filter runners such as `pandoc.utils.run_lua_filter`

### Render-time filters

Render-time filters run when Emanote renders a note to HTML:

```yaml
pandoc:
  filters:
    render:
      html:
        - filters/slides.lua
```

Use render-time filters for:

- raw HTML, CSS, or JavaScript
- writer-specific filters that branch on `FORMAT == "html"`
- diagrams and other generated assets
- calls to external tools
- filesystem, process, cache, media, or module-loading work

Render-time filters keep parsing fast because their IO work is paid only when Emanote is producing HTML.

## Filter paths

Filter paths are resolved against your notebook layers first, then against Emanote's default layer.

That means:

- `filters/custom.lua` works when the file exists in your notebook
- `lua-filters/list-table.lua` and `lua-filters/wordcount.lua` work without copying anything into your notes
- multiple filters run in declaration order

This page chains the bundled `list-table.lua` and `wordcount.lua`, and you can see the wordcount footer at the bottom.

## Org notes

Org notes use an Org keyword. Add one `#+PANDOC_FILTERS:` line per parse-time filter:

```org
#+PANDOC_FILTERS: lua-filters/list-table.lua
#+PANDOC_FILTERS: lua-filters/wordcount.lua
```

## Hot reload

Edits to `.lua` files hot-reload. The live server re-parses every note that references a changed filter, with no `touch` of the note required.

Hot reload also covers missing-at-parse-time filter references:

1. Declare a filter in frontmatter before creating it on disk.
2. Create the `.lua` file.
3. Every dependent note re-parses when the file lands.

`.lua` files are recognised as filters for hot-reload and remain linkable as source files. See [[embed|Embedding]] for a source-file embed example.

## Limitations

> [!warning] Remaining limitations
> - Filter declarations are note-local: Markdown frontmatter or Org `#+PANDOC_FILTERS:` keywords. Cascading `pandoc.filters` from an ancestor `index.yaml` is still tracked under [#263](https://github.com/srid/emanote/issues/263).
> - Parse-time filters run with `FORMAT == "markdown"`. Writer-specific HTML filters should use `pandoc.filters.render.html`, which runs with `FORMAT == "html"` and receives the note's effective metadata in `doc.meta`.
> - Filters that need filesystem, process, media, module-loading, or dynamic-code IO belong under `pandoc.filters.render.html`, not `pandoc.filters.parse`.

## Bundled filters

Two curated filters ship in Emanote's default layer under [`emanote/default/lua-filters/`](https://github.com/srid/emanote/tree/master/emanote/default/lua-filters):

- [`list-table.lua`](https://github.com/srid/emanote/blob/master/emanote/default/lua-filters/list-table.lua) turns nested bullet lists into HTML tables. It is bundled from the maintained [pandoc-ext/list-table](https://github.com/pandoc-ext/list-table) extension.
- [`wordcount.lua`](https://github.com/srid/emanote/blob/master/emanote/default/lua-filters/wordcount.lua) appends a `N words · M characters` footer to the document. This Emanote-specific filter is derived from the retired [`pandoc/lua-filters` wordcount filter](https://github.com/pandoc/lua-filters/tree/master/wordcount); upstream prints to stdout and calls `os.exit(0)`, which would terminate the live server.

## Local demo filter

This docs notebook also includes a local custom filter:

- [`slides.lua`](https://github.com/srid/emanote/blob/master/docs/filters/slides.lua) turns a `:::slides` div into a navigable Markdown presentation at HTML render time, used by [[lua-filters/slides]].

## Demos

### `list-table.lua`

::: {.list-table}
   * - row 1, column 1
     - row 1, column 2
     - row 1, column 3

   * - row 2, column 1
     -
     - row 2, column 3

   * - row 3, column 1
     - row 3, column 2
     - Well!
:::

### `wordcount.lua`

The footer at the bottom of this page is emitted by parse-time `wordcount.lua` — every save recomputes it.

### `slides.lua`

See [[lua-filters/slides]] for a full Markdown presentation _about_ Lua filters, rendered by this notebook's local `filters/slides.lua` with `FORMAT == "html"`.
