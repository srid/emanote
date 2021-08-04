# emanote

[![AGPL](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://en.wikipedia.org/wiki/Affero_General_Public_License)
[![built with nix](https://img.shields.io/badge/Built_With-Nix-5277C3.svg?logo=nixos&labelColor=73C3D5)](https://builtwithnix.org)
[![FAIR](https://img.shields.io/badge/FAIR-pledge-blue)](https://www.fairforall.org/about/)
[![Matrix](https://img.shields.io/matrix/neuron:matrix.org)](https://app.element.io/#/room/#neuron:matrix.org "Chat on Matrix")
[![Liberapay](https://img.shields.io/liberapay/patrons/srid.svg?logo=liberapay)](https://liberapay.com/srid/donate "Donate using liberapay")

WIP: Spiritual successor to [neuron](https://neuron.zettel.page), based on [Ema](https://ema.srid.ca).

Create beautiful websites -- such as personal webpage, blog, wiki, Zettelkasten, notebook, knowledge-base, documentation, etc. from future-proof plain-text notes and arbitrary data -- with live preview that updates in real-time.

**Project Status**: Progressed enough to be usable for *certain* use-cases (see examples below). HTML templates are yet to be finalized (so do not customize your templates just yet), and most importantly folgezettel graph and visualization needs to be implemented to act as true neuron replacement (see tasks below).

## Installing and using

```bash
# Install (`cachix use srid` for cached binaries)
nix-env -if https://github.com/srid/emanote/archive/refs/heads/master.tar.gz
# Or, from the Git repo: nix-env -if ./default.nix

# Run live server (PORT is optional)
cd /path/to/notebook
PORT=8001 emanote

# Generate static files
mkdir /tmp/output
emanote -C /path/to/notebook gen /tmp/output
```

### Examples

Emanote is suitable for creating ...
* ... **project** sites, such as: [ema.srid.ca](https://ema.srid.ca) (view [source](https://github.com/srid/emanote/tree/master/docs)).
* ... **wiki** sites, such as: [Unofficial r/TheMotte Wiki](https://themotte.zettel.page/) (view [source](https://github.com/Kuratoro/TheMotte.zettel.page))
* ... **personal website**/**blogs**, such as: [www.srid.ca](https://www.srid.ca/) (view [source](https://github.com/srid/www.srid.ca))
* ... **Zettelkasten** sites: *coming soon*

## Developing

Emanote is written in Haskell. Thanks to Nix, this repository is pre-configured to provide a delightful development experience with full IDE support in Visual Studio Code. Follow these steps:

- [Install Nix](https://nixos.org/download.html) & [enable Flakes](https://nixos.wiki/wiki/Flakes)
- Run `nix-shell --run haskell-language-server` to sanity check your environment (Expect it to download and build a bunch of things the first time)
- Open the repository [as single-folder workspace](https://code.visualstudio.com/docs/editor/workspaces#_singlefolder-workspaces) in Visual Studio Code
    - Install the [workspace recommended](https://code.visualstudio.com/docs/editor/extension-marketplace#_workspace-recommended-extensions) extensions
    - <kbd>Ctrl+Shift+P</kbd> to run command "Nix-Env: Select Environment" and select `shell.nix`. The extension will ask you to reload VSCode at the end.
- Press <kbd>Ctrl+Shift+B</kbd> in VSCode, or run `bin/run` (`bin/run-via-tmux` if you have tmux installed) in terminal, to launch the dev version Emanote on `./docs`, then navigate to http://localhost:9010/
  - Changing the Haskell sources will recompile and reload this instance automatically.

All but the final step need to be done only once. See [architecture.md](docs/architecture.md) for a high-level overview of the codebase.

## Discussion

To discuss the emanote project, [join Matrix][matrix] or post in [GitHub Discussions][ghdiscuss].

[matrix]: https://matrix.to/#/#neuron:matrix.org
[ghdiscuss]: https://github.com/srid/emanote/discussions

## Tasks

(Archived/Done tasks further below)

### Current

Before tests (tasks impacting the larger architectural context in code base),

- Zettelkasten / Graph considerations
  - [ ] Incrementally build a graph of notes using [algebraic-graphs-patch](https://github.com/srid/emanote.obelisk/tree/master/lib/algebraic-graphs-patch)
  - [ ] Using the graph, produce a folgezettel index (z-index) at `/-/folgezettel.html`
  - [ ] Using the graph, reinstate neuron UpTree on top of each note
  - [x] Reinstate Neuron look & feel (or just improve on it; make Emanote's note template distinct-looking, and an improved version of neuron's)
  - [ ] UI considerations (re: dirtree vs folgezettel heterarchy)
- [ ] Finally, **tests**!
  - URL parsing (.md and wiki-links) and route encoding/decoding
  - Metadata overriding

To triage,

- [ ] `neuron query` equivalent?
- [ ] Consistent ordering of notes in sidebar, index, query listing, backlinks
  - `.timeline` query in particular should be sort by date
  - calendar children shoiuld be sorted by name (thus day), just as other notes?
- [ ] fsnotify: reliably handle directory renames/ moves
  - Straightforward to do using unionMount's OverlayFs?
  - If nothing, restart mount on such events.
- [ ] RSS

Before public release

- [ ] Finalize Heist variables/structures and template locations
  - Try Heist.Compiled
- [ ] Finalize Tailwind styling of Pandoc (esp. margins)

### Archived Tasks

Initial MVP,

- [x] Wiki-links
- Splice work
  - [x] Make sidebar tree a splice
  - [x] Make breadcrumbs a splice
    - Requires supporting arbitrary HTML in node children
  - [x] Make pandoc view a splice
- [x] Backlinks
  - Using ixset
- [x] Report error on web / CLI on markdown parse failure (generally on any error)
- [x] .emanote/templates/settings.yml - to pass global vars (`theme`, `site-title`) as-is
- [x] Use default templates and metadata if none exist
  - [x] Load templates from cabal data-files by default
  - [x] Do the same for `index.yaml` (then test on haskell-kb)
- [x] Use default static files (favicon.svg) for those that do not exist
- [x] Finish Pandoc AST rendering (address Unsupported)
- [x] Add docker image
- [x] Milestone: Make ema.srid.ca an emanote site
  - Bugs and blockers
    - [x] /start.md - the .md breaks links
    - [x] workaround raw html bug (see below) using video raw format
    - [x] "Next" styling, via class map in .yaml
  - [x] docs: adjust tutorial for new ema-template 
  - [x] ema-docs: replace with ema-template
- [x] Tailwind CDN: replace with windi workflow for faster page load, or use Twind shim
- [x] Avoid "Ema - Ema" kind of title. Pass ifIndexRoute splice?
- [x] BUG: /Haskell.org (with dot in it) crashes ema dev server
- [x] Milestone: `./emanote -C ~/code/haskell-knowledge-base` should just work.
- [x] apply prismJS on live server refresh?
- [x] Add fsnotify watcher for default template files (etc), but only in ghcid mode
- [x] Sidebar: expand-by-default on per-tree basis, by enabling it on yaml or frontmatter
- [x] Directory routes (allow `$dir.html` even if `$dir.md` doesn't exist)
- [x] Finalize in HTML templating: heist vs a more popular one?
  - Probably gonna take the heist trade-off, given the ability to customize breadcrumbs/sidebar/pandoc HTML

Milestone (www.srid.ca),

- [x] Footnotes
- [x] Custom route slugs https://github.com/srid/emanote/discussions/42
- Blog post friendly
  - Queries
    - [x] Query by tag (in code block)
    - [x] Date in queries (requires Heist withJson changes)
		- [x] Hierarchical tags?
- Theme touches
  - [x] Timeline query styling (use CSS grid)
  - [x] Ugly footnote empty line with multi-block notes
  - [x] Final website look (not boring)
- [x] Interlude(architecture): a layer between ema and emanote
  - source -> target file transformation with routing
  - examples
    - source: .md, .org, static files, ..
    - output: .rss/.xml
- [x] WikiLink: allow linking to non-HTML files.
  - [x] Refactor `R` to accomodate them all, and ditch `Either FilePath`
  - [x] Try `OpenUnion` to make Note/Ref's route field polymorphic over file type
- Embedding / Filtering / Transforming / etc
  - [x] Link embedding: support `![[]]` of Obsidian? https://help.obsidian.md/How+to/Embed+files
    - Consider designing this in the larger context of Pandoc splice with customizable rendering 
      - Including wiki-links (thus supplanting rewriteLinks)
      - Including queries (see below)
    - Also consider non-Obsidian formats, `![[program.hs:2-13]]
  - [x] Queries and results embed (see below)
  - [x] WikiLink embedding for note should use .tpl layout