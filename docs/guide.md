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

- notebook layers overlay
- polymorphic (in filetype, and path signifier) wiki-links: `[[Foo]]`, `[[Bar/Qux]]`, `[[Examples/Program.py]]`
- linking to 'folders' (even without $folder.md)
- [data cascade](https://www.11ty.dev/docs/data-cascade/)
- s/[shortcodes](https://web.dev/how-we-build-webdev-and-use-web-components/#templating)/attribute class/ (i., `:::{.foo}`)
- Powerful and simpler query system (cf. Obsidian search)
  - Fully customizable 'results' layout (eg: to produce blog timeline with summary snippet)
- Obsidian-style embedding
  - Describe a workflow of daily note + note context (alternative to Andy's stacked notes)
- Pandoc filters (`Pandoc -> IO Pandoc`)
  - Including citations
- mdBook like search (emanote should provide the index)
- Ref: [top requested neuron features](https://github.com/srid/neuron/issues?q=is%3Aissue+is%3Aopen+sort%3Areactions)
- Tailwind styling in Markdown
- template hooks

Known limitations

- JS unreliability in live-server mode.
	- PrismJS works
	- MathJS does not
- Fsnotify limitations
  - If doing a directory move/rename, restart emanote.