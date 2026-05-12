---
slug: diagrams
short-title: Diagrams
pandoc:
  filters:
    render:
      html:
        - lua-filters/diagram.lua
diagram:
  cache: true
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

D2's declarative syntax is the shortest path from prose to picture. The identity-flip Richard describes in ["Something has changed in me"](https://www.actualfreedom.com.au/richard/audiotapeddialogues/somethinghaschangedinme.htm) — the move from a self defined by who-one-was and who-one-was-becoming to a self defined by what-and-who-one-actually-is — lays out as a 2×2 grid:

```d2
grid-rows: 2
grid-gap: 30

was: "Who I was" {style.fill: "#fee2e2"}
becoming: "Who I was\nbecoming" {style.fill: "#fee2e2"}
what: "What I am" {style.fill: "#dcfce7"}
who: "Who I am" {style.fill: "#dcfce7"}

was -> what: "coming to\nmy senses" {style.bold: true}
becoming -> who: "coming to\nmy senses" {style.bold: true}
```

## cetz demo

CeTZ shines on figures whose meaning is geometric, not flow-shaped. The temporal contrast Richard draws in ["This moment has no duration"](https://www.actualfreedom.com.au/richard/audiotapeddialogues/thismomenthasnoduration.htm) — _time had a periodicity_ versus _the cutting edge_ where _this moment has no duration_ — is itself a geometric distinction (discrete tick marks vs. an infinitesimally thin line) that cetz can render directly:

```cetz
#import "@preview/cetz:0.3.4": canvas, draw

#canvas({
  import draw: *

  let tick_xs = (-6.8, -5.6, -4.4, -3.2, -2.0)
  line((-7.2, 0), (-1.6, 0), stroke: 1.2pt + gray)
  for x in tick_xs {
    line((x, -0.35), (x, 0.35), stroke: 2.5pt + gray)
  }
  content((-4.4, -1.0), text(0.9em, fill: gray)[_time had a periodicity_])

  rect((2.92, -1.0), (3.08, 1.0), fill: rgb("#1d4ed8"), stroke: rgb("#1d4ed8"))
  content((3.0, 1.5), text(1em, weight: "bold", fill: rgb("#1d4ed8"))[_the cutting edge_])
  content((3.0, -1.5), text(0.85em, fill: rgb("#1d4ed8"))[_this moment has no duration_])
})
```

## Caching

The upstream filter supports a content-addressed disk cache that skips re-running an engine for unchanged source. Enable it once per document via metadata:

```yaml
diagram:
  cache: true
```

Cache files land under `$XDG_CACHE_HOME/pandoc-diagram-filter/` (typically `~/.cache/pandoc-diagram-filter/`), keyed by `sha1(fence-body)`. The cache is shared across notebooks and naturally git-clean. Set `diagram.cache-dir: <path>` to override the location.

> [!note]
> The cache key is the fence body text only — it does **not** include the engine binary version or the fence's code-block attributes. Bumping `d2` / `typst` (or changing a `{.d2 layout=elk}` attribute) does not invalidate previously-cached renders. Bust the cache manually with `rm -rf $XDG_CACHE_HOME/pandoc-diagram-filter/` when an engine upgrade should reflow output.

Without caching, every render of a page re-invokes the engine. For static [[layer|sites]] this happens once per build; for the live server it happens on each save of a note that contains diagrams.

## Limitations

- Filter declaration must live on the note itself — [[yaml-config|site-wide cascade]] from `index.yaml` is tracked in [#263](https://github.com/srid/emanote/issues/263).
- Other engines (mermaid, dot, plantuml, tikz, asymptote) work if you install the matching binary, but Emanote's Nix closure does not pin them. Set the engine's `_BIN` environment variable (`MERMAID_BIN`, `DOT_BIN`, …) to override the executable path explicitly.
- Diagrams render only when `FORMAT == "html"`, so they have no effect on parse-time concerns like backlinks, tags, search index, or the note model. The fenced source remains in the document for export targets that don't run the filter.
