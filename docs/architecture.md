---
order: 99
---

# Architecture

Emanote transforms a bunch of "source files" (Markdown, static files, etc.) into a "target website", and does so in a *reactive* manner such that as the source files change the resultant website updates in real-time. The hot-reload aspect is delegated to [Ema](https://ema.srid.ca/), but Emanote's high-level architecture is as follows,

- `Emanote.Source`: manages source files and communicates them to `Emanote.Model`.
  - The key concept here is the notion of "union mount", implemented by `Emanote.Source.Mount`, which will eventually be made its own Haskell library.
- `Emanote.Model`: Haskell types & machinary to represent the source files in memory, as well as a way to efficiently index and query into them.
  - The individual modules in this package should be `import`ed separately to use the specific model types.
- `Emanote.Route`: Haskell route types to point to somewhere in `Emanote.Model` (eg: route to a .md file, or a route to a .jpeg file)
- `Emanote.View`: Rendering code (HTML, templates, site routes)

Ema acts as the framework orchestrating two things at the same time: 

1. `Emanote.Source.run` which is responsible for keeping the in-memory `Model` in sync with what's on disk
1. `Emanote.View.render` which, whilst querying the `Model`, is responsible for producing the final HTML for every `SiteRoute` in the target website.

In addition, we have the following non-source files in the Git repository that are vital to Emanote's functionality:

- `./default`: The primary and first "layer" used, which provides the default HTML templates (we use Heist), `index.md` and a favicon. Users can override these by creating an equivalent file (same path) in their own layer.
