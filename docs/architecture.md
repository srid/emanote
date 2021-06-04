---
order: 99
---

# Architecture

Emanote transforms a bunch of "source files" (Markdown, static files, etc.) into a "target website", and does so in a *reactive* manner such that as the source files change the resultant website updates in real-time. The hot-reload aspect is delegated to [Ema](https://ema.srid.ca/), but Emanote's high-level architecture is as follows,

- `Emanote.Source`: manages source files and communicates them to `Emanote.Model`.
- `Emanote.Model`: Haskell types & machinary to represent the source files in memory, as well as a way to efficiently index and query into them.
  - The individual modules in this package should be `import`ed separately to use the specific model types.
- `Emanote.Route`: Haskell route types to point to somewhere in `Emanote.Model` (eg: route to a .md file, or a route to a .jpeg file)
- `Emanote.View`: Rendering code (HTML, templates, site routes)

In addition, we have the following non-source files in the Git repository that are vital to Emanote's functionality:

- `./default`: The primary and first "layer" used, which provides the default HTML templates (we use Heist), `index.md` and a favicon. Users can override these by creating an equivalent file (same path) in their own layer.
