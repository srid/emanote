---
order: -90
---
# Full-text search

Emanote provides client-side full-text search using [Stork](https://stork-search.net/)[^1]. Search should work in both live server and the statically generated site.

You can trigger search input in one of the following ways:

- Press `Ctrl+K` (or `⌘K` on macOS).
- Click the lens icon on the sidebar 
  - If the sidebar is disabled (eg.: you are on [[neuron-layout]]), the search lens icon will be on the top-right corner.

## Including frontmatter in the search index

By default, Stork doesn't include the text in the frontmatter as searchable.

This can be changed by adding the following to [[yaml-config|`index.yaml`]]:

```yaml
template:
  stork:
    frontmatter-handling: ignore
```

The possible values are `ignore`, `omit` and `parse`. Default `omit`. See the Stork [docs](https://stork-search.net/docs/config-ref#frontmatter_handling).

This allows you to search by your frontmatter metadata (eg: tags) instead of just markdown body. Note that you may also get undesired results when searching words including non-relevant parts of the YAML frontmatter, like `order` or `date`; this is why the default is `omit`.


## Known issues

- In live server mode, you may find that the browser will fetch remote assets (the wasm file) from files.stork-search.net. See details [here](https://github.com/jameslittle230/stork/issues/317#issuecomment-1258682222).

[^1]: The Stork index file can be accessed at [`-/stork.st`](-/stork.st).
