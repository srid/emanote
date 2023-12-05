---
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

> [!warning] Limitations
> See [here](https://github.com/srid/emanote/pull/278#issue-1207537343) for known limitations.

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
