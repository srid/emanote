---
tags: [emanote/syntax/demo]
---

# Embedding

You can embed files, using `![[..]]` - a syntax inspired by [Obsidian](https://help.obsidian.md/How+to/Embed+files). The HTML can be fully customized for each embed types.

## Notes

Embedding a note will simply inline it. For example, using `![[start]]` displays the following:

![[start]]

## Files

Embedding of [[file-links]], as indicated in the aforementioned Obsidian help page, will eventually be supported; for now, certain file types already work.

See https://github.com/srid/emanote/issues/24 for progress.
### Images 

Embedding image files as, say, `![[disaster-girl.jpg]]` is equivalent to `![](path/to/disaster-girl.jpg))`. 

![[disaster-girl.jpg]]

### Videos

The following is the result of using `![[death-note.mp4]]`.

![[death-note.mp4]]
