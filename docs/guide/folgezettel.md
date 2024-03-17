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

{#folder}
## [[folder-note|Folder notes]]

By default, Emanote includes any directories in your note's path as vertices in the folgezettel graph. This makes the [[folder-note|folder note]] a folgezettel parent of the child note. The contents of a folder, likewise, become folgezetten children of the [[folder-note|folder note]]. For eg., in `foo/bar/qux.md`, "foo" is a folgezettel parent of "bar", and "bar" is a folgezettel parent of "qux".

>[!note]
> The top-level root folder is not considered a folgezettel by default, unless you specify it (as shown below). This is to support the use-case of the user having a *flat list* of notes *without* subdirectories, but connected through [[folgezettel]].

This behavior can be configured. To turn it off on a per-folder basis, set the corresponding flag in [[yaml-config|your configuration]] to `false` as shown here:

```yaml
emanote:
  # Whether to automatically treat the contents of this 
  # folder note as its folgezettel children
  folder-folgezettel: false
```

>[!tip]
> Put this configuration in `foo.md` if you want to disable folder folgezettel for just that [[folder-note|folder note]] `foo/`. Put it in `foo.yaml` if you want to disable it for `foo/` and all subfolders under it recursively. Put it in top-level `index.yaml` to disable it for all folders. See [[yaml-config]] for more details.