# Bundled Pandoc Lua Filters

This directory contains the curated Lua filters Emanote makes available from
the default layer. Notebook authors can reference them from Markdown
frontmatter without copying files into their own notebook:

```yaml
pandoc:
  filters:
    - lua-filters/list-table.lua
    - lua-filters/wordcount.lua
```

Emanote searches user notebook layers first. A user-provided file at the same
path named in frontmatter, such as `lua-filters/list-table.lua`, therefore
overrides this bundle.

## Included Filters

- `list-table.lua`: from `pandoc/lua-filters` `list-table/`.
- `wordcount.lua`: adapted from `pandoc/lua-filters` `wordcount/` for
  Emanote's live server. The upstream filter prints to stdout and exits the
  process; this bundled version appends a rendered footer instead.

The upstream source revision used for this curation pass was
`818f901a56cff8629dbd3d3d2d719320279ef6d1`.
