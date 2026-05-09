---
slug: embed
tags: [emanote/syntax/demo]
date: 2022-08-03
---

# Embedding

You can embed files, using `![[..]]` - a syntax inspired by [Obsidian](https://help.obsidian.md/Linking+notes+and+files/Embedding+files). The HTML can be fully customized for each embed types.

> [!warning] 
> The embed wiki-link syntax must appear on a paragraph of its own, with no other text added next to it.[^blk] Recursive embeds are supported (see [#cyclic-embeds] below for the one safety stop).

[^blk]: This constraint is necessary to ensure that the HTML generated remains valid. Embedded content use block elements, which cannot be embedded inside inline nodes.

## Notes

Embedding a note will simply inline it. For example, using `![[start]]` displays the following:

![[start]]


## Cyclic embeds

Embeds compose recursively — an embedded note may itself embed further notes — but a chain that closes back on itself (`a → b → a`, or a note that embeds itself) cannot be expanded without a fixpoint. Emanote detects the cycle and substitutes an inline `↺ Cyclic embed: <title> (via …)` placeholder at the point the loop would close, naming both the offending note and the chain that led there.

The note [[cyclic-embed-demo]] embeds itself. Embedding it here renders one level of its content, then the placeholder where the inner self-embed would have resolved:

![[cyclic-embed-demo]]


## Files

Embedding of [[file-links]], as indicated in the aforementioned Obsidian help page, will eventually be supported; for now, certain file types already work.

> [!tip] Progress
> See https://github.com/srid/emanote/issues/24 for progress on this feature.

### Images

Embedding image files as, say, `![[disaster-girl.jpg]]` is equivalent to `![](path/to/disaster-girl.jpg)` (this example links to [[disaster-girl.jpg|this image]]).  

> [!tip] See also 
> [[adding-images]].

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

Source-code, markup, and configuration files can be embedded using the same syntax. The file's extension is matched against [skylighting](https://github.com/jgm/skylighting)'s bundled syntax map, and the content is highlighted at build time through the same pipeline used for fenced code blocks (see [[syntax-highlighting]]) — so an embedded `.hs` file looks identical to the equivalent ` ```haskell ` block.

The following is the result of using `![[haskell-code.hs]]` (the regular Markdown form `![](haskell-code.hs)` also works):

![[haskell-code.hs]]

A C file:

![[c-code.c]]

A JSON file:

![[data-demo.json]]

A TOML file:

![[config-demo.toml]]

A CSS snippet:

![[styles-demo.css]]

#### Supported extensions

Anything skylighting's [`syntaxesByExtension`](https://hackage.haskell.org/package/skylighting-core/docs/Skylighting-Core.html#v:syntaxesByExtension) recognises will highlight — that's hundreds of languages spanning Ada through Zsh, including the common programming languages, shells, markup (`.html`, `.tex`, `.rst`, …), data formats (`.json`, `.yaml`, `.toml`, `.xml`, …), and config files (`.ini`, `.css`, `.scss`, …). The image, audio, video, and PDF extensions listed above take precedence and embed via their dedicated templates instead.

To add support for a new language, contribute a Kate XML syntax file upstream to skylighting — Emanote picks it up automatically the next time the dependency is bumped.
