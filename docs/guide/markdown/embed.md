---
tags: [emanote/syntax/demo]
date: 2022-08-03
---

# Embedding

You can embed files, using `![[..]]` - a syntax inspired by [Obsidian](https://help.obsidian.md/Linking+notes+and+files/Embedding+files). The HTML can be fully customized for each embed types.

Note that the embed wiki-link syntax must appear on a paragraph of its own, with no other text added next to it.[^blk] Recursive embeds are supported.

[^blk]: This constraint is necessary to ensure that the HTML generated remains valid. Embedded content use block elements, which cannot be embedded inside inline nodes.

## Notes

Embedding a note will simply inline it. For example, using `![[start]]` displays the following:

![[start]]


## Files

Embedding of [[file-links]], as indicated in the aforementioned Obsidian help page, will eventually be supported; for now, certain file types already work.

See https://github.com/srid/emanote/issues/24 for progress.
### Images

Embedding image files as, say, `![[disaster-girl.jpg]]` is equivalent to `![](path/to/disaster-girl.jpg)` (this example links to [[disaster-girl.jpg|this image]]).  See also the tip: [[adding-images]].

[![[disaster-girl.jpg]]](https://knowyourmeme.com/memes/disaster-girl)

It is also posible to add images inline (example, here's the site favicon: [![[favicon.svg]]]{.w-6}) say in the middle of a paragraph.

### Videos

The following is the result of using `![[death-note.mp4]]` (note that `![](death-note.mp4)` also works).

![[death-note.mp4]]

### Audio

The following is the result of using `![[cat.ogg]]` (note that `![](cat.ogg)` also works).

![[cat.ogg]]

### PDFs

PDFs can be embedded using the same syntax. The following is the result of using `![[git-cheat-sheet-education.pdf]]` (note that `![](git-cheat-sheet-education.pdf)` also works):

![[git-cheat-sheet-education.pdf]]

### Code files

Code files can be embedded using the same syntax. The following is the result of using
`![[haskell-code.hs]]` (note that `![](haskell-code.hs)` also works):

![[haskell-code.hs]]

A C file:

![[c-code.c]]

Right now these are the code file extensions supported:

- `.hs`
- `.sh`
- `.py`
- `.js`
- `.java`
- `.cpp`
- `.cs`
- `.rb`
- `.go`
- `.swift`
- `.kt`
- `.rs`
- `.ts`
- `.php`
- `.c`

To include a new one please see [here](https://github.com/srid/emanote/pull/444). Please
note that the wrong syntax highlighting might be applied if not using Chrome.
