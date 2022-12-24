---
order: -90
---
# Full-text search

Emanote provides client-side full-text search using [Stork](https://stork-search.net/)[^1]. Search should work in both live server and the statically generated site.

You can trigger search input in one of the following ways:

- Press `Ctrl+K` (or `⌘K` on macOS).
- Click the lens icon on the sidebar (if using the 'book' [[html-template|template layout]]) or top-right corner (if using [[neuron-layout]]).

## Including frontmatter content in the search

By default, Stork doesn't include the text in the frontmatter as searchable.

This can be changed by adding the following to `index.yaml`:

```yaml
template:
  stork:
    frontmatter-handling: ignore
```

This allows you to search by the tags present in the frontmatter, but you will also get potentially undesired results when searching words like `order` or `date`.

The possible values are `ignore`, `omit` and `parse`. Default `omit`. See the Stork [docs](https://stork-search.net/docs/config-ref#frontmatter_handling).

## Known issues

- In live server mode, you may find that the browser will fetch remote assets (the wasm file) from files.stork-search.net. See details [here](https://github.com/jameslittle230/stork/issues/317#issuecomment-1258682222).

[^1]: The Stork index file can be accessed at [`-/stork.st`](-/stork.st).
