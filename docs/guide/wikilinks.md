---
slug: wikilinks
---

# Wiki Links

You can link to a note by placing the filename (without extension) inside double square brackets. For example, `[[neuron]]` links to the file `neuron.md` and renders as [[neuron]]. The link uses the target note's title automatically; specify a custom title with `[[neuron|Moving off neuron]]` (renders as [[neuron|Moving off neuron]]) or force use of the filename with `[[neuron|neuron]]` (renders as [[neuron|neuron]]).

## Structural links

See [[folgezettel]] for the special wiki-link form used to declare the [[sidebar]] (and [[uptree]]) hierarchy.

## Anchors

Wiki-links [do not yet](https://github.com/srid/emanote/discussions/105) support anchor links. Regular Markdown links do — for example, [example link](./markdown.md#lists).

## Regular Markdown note links

Regular Markdown links to notes can include their `.md` or `.org` extension, but they don't have to. `[Neuron](../start/neuron)` and `[Neuron](../start/neuron.md)` both render as [Neuron](../start/neuron). Folder-note links such as `[Guide](../guide)` resolve the same way before Emanote falls back to looking for a static file at that path.

## Broken links

Broken links render with a distinctive red/error style so missing notes stand out. For example: [[Foo bar]] (wiki-link) and [Foo bar](foo-bar.md) (Markdown link). Fix by creating the target file or correcting the link path.

The default rendering carries semantic class hooks — `emanote:broken-link` on the root span, `emanote:broken-link__text` on the strikethrough, and `emanote:broken-link__icon` on the X — so a notebook stylesheet can hide the icon or swap the strikethrough for an underline without replacing the template. To change structure, drop a `templates/components/broken-link.tpl` into your notebook (see [[html-template]]); it overrides the default and receives a single `<ema:broken-link:text />` splice carrying the source text of the offending link ([#221](https://github.com/srid/emanote/issues/221)). The broken-link landing page (where clicking the link leads) is the existing `templates/error.tpl` and is overridden the same way.

## Ambiguous links

Ambiguous wiki-links are disambiguated by selecting the one that shares the closest ancestor.[^ambig] When no candidate is closer, the link is left unresolved and a list of routes is shown next to it in live preview so you can pick one.

The default rendering goes through `templates/components/ambiguous-link.tpl` (see [[html-template]]) and carries `emanote:ambiguous-link[__text|__icon|__candidate]` class hooks. Notebook authors can drop in their own template; the override receives `<ema:ambiguous-link:text />` plus a `<ema:ambiguous-link:candidates><each-candidate>…</each-candidate></ema:ambiguous-link:candidates>` list-splice with per-candidate `<ema:candidate:label />`, `${ema:candidate:url}`, `${ema:candidate:tooltip}`, and a boolean `<ema:candidate:newtab><then>…</then><else>…</else></ema:candidate:newtab>` splice that fires `<then>` for non-Ema-internal candidates so the template can decide what `target="_blank"` means ([#712](https://github.com/srid/emanote/issues/712)).

[^ambig]: This particular selection process [was chosen](https://github.com/srid/emanote/pull/498) in particular to allow combining multiple notebooks (with similar note filenames) at the top-level.
