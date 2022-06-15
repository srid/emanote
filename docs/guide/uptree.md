# Uplink tree

The _uplink tree_ or "uptree"[^neuron] is a feature available in #[[neuron-layout|the `note` template]], which visualizes hierarchical relationships between notes as a graph at the top of the page.

[^neuron]: This feature was inspired by an equivalent feature [from Neuron](https://neuron.zettel.page/uplink-tree).

These relationships are called by _folgezettel links_ and created with a special variation of emanote link syntax.

Conceptually, a [folgezettel](https://neuron.zettel.page/folgezettel-heterarchy) link can be thought of as an arrow from a parent note to a descendent note.

## Syntax

```markdown
## A folgezettel link from another page to this one:

Here is the #[[source-note]]

## A folgezettel link from this page to another one:

Here is the [[target-note]]#
```

## Folder nodes

By default, Emanote also includes any directories in your note's path as vertices in the uptree graph.

This behavior can be configured. To turn off the implicit folder nodes, set to `false` the corresponding flag in [[yaml-config|your configuration]] as shown here:

```yaml
emanote:
  # Whether to automatically treat folder notes as a folgezettel parent of its contents
  folder-folgezettel: false
```

