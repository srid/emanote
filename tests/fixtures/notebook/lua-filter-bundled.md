---
pandoc:
  filters:
    parse:
      - lua-filters/list-table.lua
---

# Bundled Lua Filter Demo

::: {.list-table .bundled-list-table}
   * - bundled row 1, column 1
     - bundled row 1, column 2

   * - bundled row 2, column 1
     - bundled row 2 links to [[subfolder/sibling|a sibling note]]
:::
