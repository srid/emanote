# Migrating from neuron

To use Emanote in your existing notebook,

1. Configure it to use [[neuron-layout]] (this is optional)
1. In all notes, replace neuron's `z:zettels`-style [tag queries](https://neuron.zettel.page/tag-queries) with Obsidian-style queries. See [[demo|see demo]].
    - Note: Query results do not impact graph connections (thus backlinks). If you want to establish connections to multiple notes, do it by explicitly linking to them individually. Emanote chose this option, because it was simpler to implement. Users are encouraged to try to persuade the author otherwise if there is a compelling rationale.
1. If you have a `head.html`, transfer its contents to [[yaml-config|index.yaml]] ([see example and explanation](https://github.com/srid/emanote/discussions/116))

Capabilities Emanote provides, but neuron does not:

- Live server with hot reload
- Faster incremental build
- More lenient handling of malformed Markdown (eg: broken links)
- Flexible WikiLinks (link to directories, or based on path, eg: `[[Foo/Bar]]`) including [[file-links]].
- Static files can be placed anywhere (not just in `./static` folder)
- Full path based URLs (unless `slug` is set)
- Hierarchical tag index at `/-/tags.html`.
- Full theming (customize HTML fully)
- [[yaml-config|YAML based configuration]] at per-route level
- Layers (merge Zettelkastens, or overlay them)
