---
slug: lua-filters
short-title: Lua Filters
pandoc:
  filters:
    - filters/list-table.lua
---

# Pandoc Lua Filters

**WARNING**: This is an ==🧪 experimental 🧪== feature and may change in future. It is being made available so users can try it out and give feedback to the author.

> [!tip] Progress
> See https://github.com/srid/emanote/issues/263

To enable a [Pandoc Lua filter](https://pandoc.org/lua-filters.html) for a particular Markdown file, put that filter in your notebook and add the following to the Markdown file's YAML frontmatter:

```yaml
pandoc:
  filters:
    - path/to/your.lua
```

The filter path is resolved against your notebook layers, so a path like `filters/list-table.lua` works as long as `filters/list-table.lua` exists in any of your `-L`'d layers.

Edits to the `.lua` file hot-reload: the live server re-parses every note that references it the next time the filter changes on disk, no `touch` of the note required. `.lua` files are not copied to `_site/` — they are recognised as filters, not static assets.

> [!warning] Remaining limitation
> Filters can only be declared in a note's own frontmatter. Cascading `pandoc.filters` from an ancestor `index.yaml` is still tracked under [#263](https://github.com/srid/emanote/issues/263).

## Demo

This uses the [list table](https://github.com/pandoc/lua-filters/tree/master/list-table) filter (copied as [[list-table.lua]]):

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
