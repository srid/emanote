---
order: 2
---

# Guide

TODO Just a skeleton for now.

Remember to document these

- pandoc -> rewriteClass
- Heist docs for Ema
  - Helper.Heist
  - Helper.Heist.Tailwind - for `<Tailwind-Include />` in head that uses inline CSS in dev server, and include of generated CSS in prod.
  - adding custom splices (when using as a library)

Mega features,

- [data cascade](https://www.11ty.dev/docs/data-cascade/)
- Powerful and simpler query system (cf. Obsidian search)
  - Fully customizable 'results' layout (eg: to produce blog timeline with summary snippet)
- Pandoc filters (`Pandoc -> IO Pandoc`)
  - Including citations
- mdBook like search (emanote should provide the index)
- Ref: [top requested neuron features](https://github.com/srid/neuron/issues?q=is%3Aissue+is%3Aopen+sort%3Areactions)

Known limitations

- JS won't eval (properly) in live-server mode.
- Fsnotify limitations
  - If doing a directory move/rename, restart emanote.