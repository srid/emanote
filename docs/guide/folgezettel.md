---
slug: folgezettel
---

# Folgezettel links

Wikilinks can be of special type called a "folgezettel". They are created with a special variation of emanote link syntax.

Conceptually, a [folgezettel](https://neuron.zettel.page/folgezettel-heterarchy) link can be thought of as an arrow from a parent note to a descendent note.

## Syntax

```markdown
## A folgezettel link from another page to this one:

Here is the #[[source-note]]

## A folgezettel link from this page to another one:

Here is the [[target-note]]#
```

## Folder notes

By default, Emanote also includes any directories in your note's path as vertices in the folgezettel graph. This makes the folder note a folgezettel parent of the child note. The contents of a folder, likewise, become folgezetten children of the folder note.

This behavior can be configured. To turn it off on a per-folder basis, set the corresponding flag in [[yaml-config|your configuration]] to `false` as shown here:

```yaml
emanote:
  # Whether to automatically treat the contents of this folder notes as its folgezettel children
  folder-folgezettel: false
```
