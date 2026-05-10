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

To enable a [Pandoc Lua filter](https://pandoc.org/lua-filters.html) for a particular Markdown file, name the filter in the Markdown file's YAML frontmatter. Use `pandoc.filters.parse` for filters that should rewrite the document immediately after Markdown parsing:

```yaml
pandoc:
  filters:
    parse:
      - lua-filters/list-table.lua
```

Use `pandoc.filters.render.html` for filters that should run when Emanote renders the note to HTML:

```yaml
pandoc:
  filters:
    render:
      html:
        - filters/slides.lua
```

The filter path is resolved against your notebook layers first, then against Emanote's default layer. That means `filters/custom.lua` works when the file exists in your notebook, while bundled filters like `lua-filters/list-table.lua` and `lua-filters/wordcount.lua` work without copying anything into your notes. Multiple filters run in declaration order — this very page chains the bundled `list-table.lua` and `wordcount.lua`, and you can see the wordcount footer right at the bottom.

Org notes use an Org keyword instead. Add one `#+PANDOC_FILTERS:` line per parse-time filter:

```org
#+PANDOC_FILTERS: lua-filters/list-table.lua
#+PANDOC_FILTERS: lua-filters/wordcount.lua
```

Edits to the `.lua` file hot-reload: the live server re-parses every note that references it the next time the filter changes on disk, no `touch` of the note required. The reverse-dependency lookup also covers _missing-at-parse-time_ filter references — declare a filter in frontmatter before creating it on disk, then create the file: every dependent re-parses when the file lands. `.lua` files are recognised as filters for hot-reload and remain linkable as source files; see [[embed|Embedding]] for a source-file embed example.

> [!warning] Remaining limitations
> - Filter declarations are note-local: Markdown frontmatter or Org `#+PANDOC_FILTERS:` keywords. Cascading `pandoc.filters` from an ancestor `index.yaml` is still tracked under [#263](https://github.com/srid/emanote/issues/263).
> - Parse-time filters run with `FORMAT == "markdown"`. Writer-specific HTML filters should use `pandoc.filters.render.html`, which runs with `FORMAT == "html"` and receives the note's effective metadata in `doc.meta`.

## Demos

Two curated filters ship in Emanote's default layer under [`emanote/default/lua-filters/`](https://github.com/srid/emanote/tree/master/emanote/default/lua-filters):

- [`list-table.lua`](https://github.com/srid/emanote/blob/master/emanote/default/lua-filters/list-table.lua) — turn nested bullet lists into HTML tables. Bundled from the maintained [pandoc-ext/list-table](https://github.com/pandoc-ext/list-table) extension.
- [`wordcount.lua`](https://github.com/srid/emanote/blob/master/emanote/default/lua-filters/wordcount.lua) — append a `N words · M characters` footer to the document. This Emanote-specific filter is derived from the retired [`pandoc/lua-filters` wordcount filter](https://github.com/pandoc/lua-filters/tree/master/wordcount); upstream prints to stdout and calls `os.exit(0)`, which would terminate the live server.

This docs notebook also includes a local custom filter:

- [`slides.lua`](https://github.com/srid/emanote/blob/master/docs/filters/slides.lua) — turn a `:::slides` div into a navigable Markdown presentation at HTML render time, used by [[lua-filters/slides]].

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
