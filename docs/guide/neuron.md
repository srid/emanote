# Migrating from neuron

TODO 

Neuron notes should mostly work, except for the below:

- No uptree or folgezettel, yet.
- Replace `z:zettels` with Obsidian-style queries ([[demo|see demo]])
  - Query results do not impact graph connections (thus backlinks). If you want to establish connections to multiple notes, do it by explicitly linking to them individually. Emanote chose this option, because it was simpler to implement. Users are encouraged to try to persuade the author otherwise if there is a compelling rationale.
- No RSS support yet.

Capabilities Emanote provides, but neuron does not:

- Live server with hot reload
- Flexible wikilins (link to directories, or based on path, eg: `[[Foo/Bar]]`)
- Full path based URLs (unless `slug` is set)
- Full theming (customize HTML fully)
- YAML based configuration at per-route level
- Layers (merge Zettelkastens, or overlay them)
