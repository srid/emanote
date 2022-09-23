---
tags: [emanote/syntax/demo]
---

# Embedding

You can embed files, using `![[..]]` - a syntax inspired by [Obsidian](https://help.obsidian.md/How+to/Embed+files). The HTML can be fully customized for each embed types.

Note that the embed wiki-link syntax must appear on a paragraph of its own, with no other text added next to it.[^blk] Recursive embeds are supported.

[^blk]: This constraint is necessary to ensure that the HTML generated remains valid. Embedded content use block elements, which cannot be embedded inside inline nodes.

## Notes

Embedding a note will simply inline it. For example, using `![[start]]` displays the following:

![[start]]


## Files

Embedding of [[file-links]], as indicated in the aforementioned Obsidian help page, will eventually be supported; for now, certain file types already work.

See https://github.com/srid/emanote/issues/24 for progress.
### Images 

Embedding image files as, say, `![[disaster-girl.jpg]]` is equivalent to `![](path/to/disaster-girl.jpg))` (this example links to [[disaster-girl.jpg|this image]]).  See also the tip: [[adding-images]].

[![[disaster-girl.jpg]]](https://knowyourmeme.com/memes/disaster-girl)

It is also posible to add images inline (example, here's the site favicon: [![[favicon.svg]]]{.w-6}) say in the middle of a paragraph.

### Videos

The following is the result of using `![[death-note.mp4]]`.

![[death-note.mp4]]


### PDFs

PDFs can be embedded using the same syntax; ie. `![[git-cheat-sheet-education.pdf]]` will show:

![[git-cheat-sheet-education.pdf]]

Or it can also be embedded using the regular image link syntax; ie. `![](git-cheat-sheet-education.pdf)` will show:

![Embedded PDF](git-cheat-sheet-education.pdf)