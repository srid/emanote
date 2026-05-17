---
slug: writing-filters
short-title: Writing filters
pandoc:
  filters:
    render:
      html:
        - filters/hello.lua
---

# Writing a Pandoc Lua filter

A filter is a Lua table whose keys are AST element constructors — `CodeBlock`, `Para`, `Header`, `Image`, `Link`, `Str`, `Span`, `Div`, `Note`, … — and whose values are handler functions. Each handler returns a replacement AST node, or `nil` to leave the input alone.

For where filters fit in Emanote's pipeline and how to enable them in [[yaml-config|frontmatter]], see [[lua-filters]].

## Two phases, one shape

The same Lua-table shape works in both phases. What changes is what's allowed:

- **Parse-time** filters run with `FORMAT == "markdown"` and rewrite the model. They can't do IO — `io`, `os`, `require`, `dofile`, `pandoc.pipe`, `pandoc.system`, etc. are runtime-banned. Good for cheap AST rewrites that should affect [[backlinks]], tags, titles, [[search]], table structure.
- **Render-time** filters run with `FORMAT == "html"` and can do IO. Good for shelling out to engines, reading the [[layer|notebook layer]], or anything that should *not* run when Emanote is only after metadata.

Either phase can match any element type and either phase can produce any AST output. The error-reporting protocol below also works identically in both.

## Hello-world

This page loads [`filters/hello.lua`](https://github.com/srid/emanote/blob/master/docs/filters/hello.lua), which matches `CodeBlock` elements whose first class is `hello` and turns each line of the body into a bullet greeting:

```hello
world
Pandoc
Lua filters
```

Source:

![[hello.lua]]

## Reporting errors to the reader

Lua gives several tools that *look* like reasonable ways to flag "this input was malformed". They land in very different places — verified empirically against the rendered output of this page:

| Tool | Filter pipeline | Other inputs on the page | Reader sees | `emanote gen` |
| --- | --- | --- | --- | --- |
| `error('msg')` | Aborts on the offending input | Not transformed; ship as raw | Top-of-page banner; failing input stays raw | Aborts |
| `emanote.error_block{...}` | Continues | Transform normally | Inline red banner exactly where the input was | Aborts |
| `warn(...)` / `pandoc.log.warn(...)` | Continues | Transform normally | Whatever the handler returns; warning to stderr | Does **not** abort |

- `error()` is right when the filter file itself is misconfigured (missing required metadata, a typo in the table, etc.) — nothing on the page is going to work.
- `warn` / `pandoc.log.warn` are for filter-author diagnostics during development. The output goes to stderr (`[WARNING] Scripting warning at …`); the reader sees nothing. Never use them as the reader-facing error surface.
- `emanote.error_block` is the right answer for *recoverable per-element* errors. Five `cetz` blocks on a page with a typo in the second: `error()` leaves a top banner + four raw code blocks; `emanote.error_block` leaves four working diagrams + one inline banner.

### `emanote.error_block`

Emanote injects an `emanote` global into every filter's Lua chunk — parse-time and render-time. The helper builds the marker `Div` so filters don't carry near-identical copies of it:

```lua
return emanote.error_block{
  title = 'cetz error',           -- bolded title
  message = engine_stderr,        -- first CodeBlock child; what `emanote gen` recovers
  source = block.text,            -- optional: original failing source
  source_class = 'cetz',          -- optional: syntax class on the source CodeBlock
}
```

The returned `Div` carries the marker class `emanote:error:lua-filter`. The static-build walks the post-filter AST for that class and aborts unless the notebook opts out via `--allow-broken-lua-filters` — which the docs site does, which is why the sad-path demo below ships in the deployed static site.

### Live sad-path demo

```hello
ERROR: typst stderr says "unclosed delimiter at line 7"
```

The block above triggers the sad path. On the live server, the rest of the page renders normally. In a `emanote gen` run without the opt-out flag, the build aborts with `typst stderr says ...` in the failure message.

## A fuller example — `slides.lua`

See [[slides]]# for a real-world render-time filter: it rewrites `:::slides` `Div` elements into a navigable Reveal.js presentation. `Div` matching, `pandoc.RawBlock` / `pandoc.RawInline` for raw HTML, frontmatter metadata via `doc.meta` — plus the embedded source.
