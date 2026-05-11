---
slug: diagrams
short-title: Diagrams
pandoc:
  filters:
    render:
      html:
        - lua-filters/diagram.lua
---

# Diagrams

Emanote bundles [`lua-filters/diagram.lua`](https://github.com/srid/emanote/blob/master/emanote/default/lua-filters/diagram.lua), a render-time [[lua-filters|Pandoc Lua filter]] that turns fenced code blocks into inline SVG. The filter is vendored from [pandoc-ext/diagram](https://github.com/pandoc-ext/diagram) and post-processed so SVG diagrams land directly in the page — no `--extract-media` pass, no JavaScript renderer, no network at view time.

## Enabling the filter

Diagrams render only on notes that opt in. Add the bundled filter to a note's render-time HTML phase in frontmatter:

```yaml
---
pandoc:
  filters:
    render:
      html:
        - lua-filters/diagram.lua
---
```

This page declares the filter itself — every fenced block below renders through the same pipeline.

Org notes use the parallel keyword from the [[lua-filters]] guide:

```org
#+PANDOC_FILTERS_RENDER_HTML: lua-filters/diagram.lua
```

## Supported engines

The bundled filter recognises seven engine classes from upstream `pandoc-ext/diagram`; Emanote's Nix closure pins the binaries for two of them, and leaves the rest as bring-your-own:

| Engine | Class | Binary | Bundled by Emanote? |
| --- | --- | --- | --- |
| [D2](https://d2lang.com/) | `d2` | `d2` | Yes |
| [CeTZ](https://github.com/cetz-package/cetz) | `cetz` | `typst` (+ pre-resolved `@preview/cetz` package) | Yes |
| Mermaid | `mermaid` | `mmdc` | No — set `MERMAID_BIN` or install on `PATH` |
| Graphviz | `dot` | `dot` | No — set `DOT_BIN` or install on `PATH` |
| PlantUML | `plantuml` | `plantuml` | No — set `PLANTUML_BIN` or install on `PATH` |
| TikZ | `tikz` | `pdflatex` (+ `inkscape` for SVG) | No |
| Asymptote | `asymptote` | `asy` | No — set `ASYMPTOTE_BIN` or install on `PATH` |

A note that fences a class without a corresponding binary renders a `Pandoc Lua filter error` banner pointing at the failed `pandoc.pipe` call, so the missing-binary case is loud, not silent.

## d2 demo

D2's declarative syntax is the shortest path from prose to picture. Four baseline feeling-states from the [actualism method](https://actualfreedom.com.au/richard/articles/thismomentofbeingalive.htm) laid out as a 2×2 grid, with the central question labelling the edge that crosses from _affective_ to _actual_:

```d2
grid-rows: 2
grid-gap: 30

bad: Feeling bad {style.fill: "#fee2e2"}
good: Feeling good {style.fill: "#fef9c3"}
happy: Happy &\nharmless {style.fill: "#dcfce7"}
excellent: Feeling\nexcellent {style.fill: "#dbeafe"}

bad -> good: notice
good -> happy: HAIETMOBA {style.bold: true}
happy -> excellent: sustain
```

## cetz demo

CeTZ shines on figures whose meaning is geometric, not flow-shaped. The method's feeling-states sit naturally as **concentric baselines**: outside is _feeling bad_, inside is _excellent_. Asking _HAIETMOBA_ moment-to-moment is the inward-pointing arrow:

```cetz
#cetz.canvas({
  import cetz.draw: *

  let levels = (
    (2.0, "feeling bad",  rgb("#dc2626")),
    (1.6, "neutral",      rgb("#ea580c")),
    (1.2, "feeling good", rgb("#16a34a")),
    (0.8, "harmless",     rgb("#0ea5e9")),
    (0.4, "excellent",    rgb("#6366f1")),
  )

  for (r, _l, col) in levels {
    circle((0, 0), radius: r, stroke: col + 0.8pt)
  }

  for (r, l, col) in levels {
    content((0, r - 0.18), text(0.55em, fill: col, l))
  }

  line((2.5, -2.2), (0.3, -0.15), stroke: 1.2pt + black, mark: (end: ">"))
  content((2.0, -2.4), text(0.65em, weight: "bold", "HAIETMOBA"))
})
```

## Caching

The upstream filter supports a content-addressed disk cache that skips re-running an engine for unchanged source. Enable it once per document via metadata:

```yaml
diagram:
  cache: true
  cache-dir: .diagrams
```

Without caching, every render of a page re-invokes the engine. For static [[layer|sites]] this happens once per build; for the live server it happens on each save of a note that contains diagrams.

## Limitations

- Filter declaration must live on the note itself — site-wide cascade from `index.yaml` is tracked in [#263](https://github.com/srid/emanote/issues/263).
- Other engines (mermaid, dot, plantuml, tikz, asymptote) work if you install the matching binary, but Emanote's Nix closure does not pin them. Set the engine's `_BIN` environment variable (`MERMAID_BIN`, `DOT_BIN`, …) to override the executable path explicitly.
- Diagrams render only when `FORMAT == "html"`, so they have no effect on parse-time concerns like backlinks, tags, search index, or the note model. The fenced source remains in the document for export targets that don't run the filter.
