---
slug: folder-note
---

# Folder notes

For every folder `foo/` in your notebook, if a `foo.md` exists alongside it, it is consider a "folder note" that is associated with the folder. Likewise, an associated `foo.yaml` servers as the [[yaml-config]] for the entire folder and its contents recursively.

The children of folder notes become its [[folgezettel|folgezettel]] children by default.

{#index}
## `index.md`

Instead of `foo.md`, you can also use `foo/index.md` (or `index.org` if using [[orgmode]]) or `foo/index.yaml`. Internally, Emanote will treat them as `foo.md` and `foo.yaml` respectively.