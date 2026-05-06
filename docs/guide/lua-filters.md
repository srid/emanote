---
slug: lua-filters
short-title: Lua Filters
pandoc:
  filters:
    - filters/list-table.lua
    - filters/wordcount.lua
---

# Pandoc Lua Filters

To enable a [Pandoc Lua filter](https://pandoc.org/lua-filters.html) for a particular Markdown file, put that filter in your notebook and add the following to the Markdown file's YAML frontmatter:

```yaml
pandoc:
  filters:
    - path/to/your.lua
```

The filter path is resolved against your notebook layers, so a path like `filters/list-table.lua` works as long as `filters/list-table.lua` exists in any of your `-L`'d layers. Multiple filters run in declaration order — this very page chains `list-table.lua` and `wordcount.lua`, and you can see the wordcount footer right at the bottom.

Edits to the `.lua` file hot-reload: the live server re-parses every note that references it the next time the filter changes on disk, no `touch` of the note required. `.lua` files are not copied to `_site/` — they are recognised as filters, not static assets.

> [!warning] Remaining limitations
> - Filters can only be declared in a note's own frontmatter. Cascading `pandoc.filters` from an ancestor `index.yaml` is still tracked under [#263](https://github.com/srid/emanote/issues/263).
> - Emanote calls `pandoc.applyFilters` with `FORMAT == "markdown"`, so filters that branch on `FORMAT` to emit writer-specific output (HTML, LaTeX) won't fire those branches. Stick to **FORMAT-agnostic** filters that operate on the AST regardless of writer. Most pure-Lua transforms (`list-table`, `include-files`, `wordcount`, the custom `slides.lua` below) are in this camp.

## Demos

Three filters live under [`docs/filters/`](https://github.com/srid/emanote/tree/master/docs/filters):

- [`list-table.lua`](https://github.com/srid/emanote/blob/master/docs/filters/list-table.lua) — turn nested bullet lists into HTML tables. From [pandoc/lua-filters](https://github.com/pandoc/lua-filters/tree/master/list-table).
- [`wordcount.lua`](https://github.com/srid/emanote/blob/master/docs/filters/wordcount.lua) — append a `N words · M characters` footer to the document. Adapted from [pandoc/lua-filters](https://github.com/pandoc/lua-filters/tree/master/wordcount); upstream calls `os.exit(0)`, which would terminate the live server, so this fork sets the count as a footer block instead.
- [`slides.lua`](https://github.com/srid/emanote/blob/master/docs/filters/slides.lua) — custom: turn a `:::slides` div into a navigable Markdown presentation, used by [[lua-filters/slides]].

### `list-table.lua`

:::list-table
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

The footer at the bottom of this page is emitted by `wordcount.lua` — every save recomputes it.

### `slides.lua`

See [[lua-filters/slides]] for a full Markdown presentation _about_ Lua filters, rendered by `slides.lua`.
