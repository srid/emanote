---
order: 2
---

# Migrating from neuron

To use Emanote in your existing [neuron](https://github.com/srid/neuron) notebook,

1. Configure it to use [[neuron-layout]]# (this is optional)
1. In all notes, replace neuron's `z:zettels`-style [tag queries](https://neuron.zettel.page/tag-queries) with [[query]].
    - Note: Query results do not impact graph connections (thus backlinks). If you want to establish connections to multiple notes, do it by explicitly linking to them individually. Emanote chose this option, because it was simpler to implement. Users are encouraged to try to persuade the author otherwise if there is a compelling rationale.
1. If you have a `head.html`, transfer its contents to [[yaml-config|index.yaml]] ([see example and explanation](https://github.com/srid/emanote/discussions/116))

## Capabilities Emanote provides, but neuron does not

- [Live server](https://ema.srid.ca/topics/live-server) (thanks to [Ema](https://ema.srid.ca/))
  - with [hot reload](https://ema.srid.ca/topics/hot-reload)
  - and faster incremental build 
- Better linking
  - Flexible WikiLinks (link to directories, or based on path, eg: `[[Foo/Bar]]`) including [[file-links]].
  - More lenient handling of malformed Markdown (eg: broken links)
- [[query]]
  - "Timeline backlinks" (Twitter-like registering of thoughts on any note)
- [[search]]
- Better hierarchical directory tree integraion
  - Full path based URLs (unless `slug` is set)
  - Static files can be placed anywhere (not just in `./static` folder)
- [[yaml-config|YAML based configuration]] at per-route level
- [[layer]] (merge Zettelkastens, or overlay them)
  - Full theming (customize [[html-template]] fully)
- Hierarchical tag index, and task index.

## Porting existing Vim plugins

See [[export]].
