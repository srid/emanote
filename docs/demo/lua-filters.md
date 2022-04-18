---
pandoc:
  filters:
    - docs/filters/list-table.lua
---

# Pandoc Lua filters

WIP

## Examples

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
