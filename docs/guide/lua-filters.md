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

A [Pandoc Lua filter](https://pandoc.org/lua-filters.html) rewrites the parsed Pandoc document before Emanote turns it into an [[html-template|HTML page]]. Filters are note-local — declare them in [[yaml-config|Markdown frontmatter]], or for [[orgmode|Org notes]] via `#+PANDOC_FILTERS_PARSE:` / `#+PANDOC_FILTERS_RENDER_HTML:` keywords.

## Choose a phase

| Phase | YAML key | Runs with | Use for |
| --- | --- | --- | --- |
| **Parse-time** | `pandoc.filters.parse` | `FORMAT == "markdown"` | Cheap AST rewrites that should affect Emanote's model — titles, tags, [[wikilinks|links]], [[backlinks]], tasks, [[markdown\|table structure]], [[search\|search text]]. **IO is banned at runtime** (`io`, `os`, `require`, `pandoc.pipe`, `pandoc.system`, etc.) so model updates stay deterministic and fast. |
| **Render-time** | `pandoc.filters.render.html` | `FORMAT == "html"` | [[html-template\|HTML-specific]] output, [[mermaid\|diagrams]], shelling out to engines, anything that should *not* run when Emanote is only after metadata. IO is allowed. |

Both phases use the same filter shape; see [[writing-filters]] for the API, the [[writing-filters#reporting-errors-to-the-reader|error-reporting protocol]], and a hello-world example.

## Filter paths

Filter paths are resolved against your [[layer|notebook layers]] first, then against Emanote's [[layer|default layer]]:

- `filters/custom.lua` works when the file exists in your notebook
- `lua-filters/list-table.lua` and `lua-filters/wordcount.lua` work without copying anything into your notes — they come from the default layer
- multiple filters run in declaration order

This page chains the bundled `list-table.lua` and `wordcount.lua` — the wordcount footer at the bottom proves it.

## Org notes

[[orgmode|Org notes]] use one keyword per filter:

```org
#+PANDOC_FILTERS_PARSE: lua-filters/list-table.lua
#+PANDOC_FILTERS_PARSE: lua-filters/wordcount.lua
#+PANDOC_FILTERS_RENDER_HTML: filters/slides.lua
```

## Hot reload

Edits to `.lua` files hot-reload. The live server re-parses every note that references a changed filter, no `touch` needed. A filter referenced before it exists on disk re-parses dependent notes the moment the file lands. `.lua` files also remain linkable as source files — see [[embed|Embedding]].

## Limitations

> [!warning]
> - Filter declarations are note-local. Cascading `pandoc.filters` from an ancestor [[yaml-config|`index.yaml`]] is tracked under [#263](https://github.com/srid/emanote/issues/263).
> - Filters that need filesystem, process, media, module-loading, or dynamic-code IO belong under `pandoc.filters.render.html`, not `pandoc.filters.parse`.

## Bundled filters

Four curated filters ship in Emanote's [[layer|default layer]] under [`emanote/default/lua-filters/`](https://github.com/srid/emanote/tree/master/emanote/default/lua-filters):

- [`list-table.lua`](https://github.com/srid/emanote/blob/master/emanote/default/lua-filters/list-table.lua) — nested bullet lists → HTML tables. From [pandoc-ext/list-table](https://github.com/pandoc-ext/list-table).
- [`wordcount.lua`](https://github.com/srid/emanote/blob/master/emanote/default/lua-filters/wordcount.lua) — appends a `N words · M characters` footer.
- [`diagram.lua`](https://github.com/srid/emanote/blob/master/emanote/default/lua-filters/diagram.lua) — fenced code blocks for `d2`, `cetz`, and the other engines [`pandoc-ext/diagram`](https://github.com/pandoc-ext/diagram) supports → inline SVG. The wrapped Emanote binary carries `d2`, `typst`, and an offline `@preview/cetz` package cache; see [[diagrams]].
- [`hello.lua`](https://github.com/srid/emanote/blob/master/emanote/default/lua-filters/hello.lua) — a hello-world filter that drives the [[writing-filters|writing-filters guide]] and exercises the error-reporting protocol. Drop `lua-filters/hello.lua` into a note's `pandoc.filters.render.html` and fence with `hello` blocks to try it.

## Local docs filters

This docs notebook also includes a custom filter under `docs/filters/`:

- [`slides.lua`](https://github.com/srid/emanote/blob/master/docs/filters/slides.lua) — turns a `:::slides` div into a navigable [[markdown|Markdown]] presentation; runs the [[slides]] deck.

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

The footer at the bottom of this page — every save recomputes it.

### `slides.lua`

See [[slides]] for a Markdown presentation rendered by `slides.lua` at HTML render time.

### `diagram.lua`

See [[diagrams]] for inline-SVG demos of `d2` and `cetz` fences, both rendered offline.

### `hello.lua`

See [[writing-filters]] — embedded source plus live happy and sad-path renders.
