# Revision history for emanote

## 1.6.0.0 (Unreleased)

**Notable features**

- **MCP server** scaffolding: new `emanote run --mcp-port PORT` flag runs an in-process Model Context Protocol HTTP endpoint beside the live server. Phase 1 ships only the lifecycle handshake and empty resource/tool inventories; richer surfaces follow in later phases ([#645](https://github.com/srid/emanote/issues/645))
- Callouts: support **nested** and **foldable** Obsidian-style callouts (`> [!type]+` / `[!type]-`), rendering as `<details>`/`<summary>` ([#465](https://github.com/srid/emanote/issues/465), [#652](https://github.com/srid/emanote/pull/652))
- **Tailwind v3 → v4 migration** with CSS-variable design tokens ([#633](https://github.com/srid/emanote/pull/633))
- Built-in static syntax highlighting using skylighting, replacing client-side JS highlighters ([#624](https://github.com/srid/emanote/pull/624))
- Built-in static math rendering (LaTeX → MathML at build time via `texmath`) ([#639](https://github.com/srid/emanote/pull/639))
- UI revamp (#622, [#636](https://github.com/srid/emanote/pull/636))
  - New self-hosted typography: Lora + Space Grotesk + Space Mono.
  - Manual dark/light theme toggle (#605, #617) with `localStorage` persistence.
  - Backlinks as a card grid; TOC with depth-based hierarchy and `IntersectionObserver` scroll-spy (#520).
  - Popup footnotes as the only on-screen UI (desktop card / mobile bottom-sheet); printed output renders the footnote list ([#642](https://github.com/srid/emanote/pull/642)).
  - Site-authored interactive JS extracted from per-template `<script>` blocks into ES modules under `_emanote-static/js/` (loaded once, cached across pages). Code-copy buttons now also appear on code blocks added by the live-server's DOM patches, not only those present at first load ([#643](https://github.com/srid/emanote/issues/643)).
  - Stork search controller migrated to the same ES-module pattern. Templates expose the search trigger via `data-emanote-stork-toggle` (event delegation) instead of inline `onclick="window.emanote.stork.toggleSearch()"`; the dark-mode mirror that re-skins the search dialog moved into the module too. Closes the Stork follow-up implied by [#643](https://github.com/srid/emanote/issues/643).
- Mermaid: add `elk` layout ([#618](https://github.com/srid/emanote/pull/618))
- Home Manager module: macOS support via launchd ([#623](https://github.com/srid/emanote/pull/623))

**Bug fixes**

- Atom feed: a feed query that matches no notes no longer crashes the build; an empty-but-valid Atom document is emitted instead. Configuration errors (missing/invalid query block, missing `page.siteUrl`) still fail loudly ([#490](https://github.com/srid/emanote/issues/490), [#650](https://github.com/srid/emanote/pull/650))
- Markdown links to a static `.xml` asset (e.g. `[Test](./test.xml)`) now resolve to the file. Previously a `.xml` URL was always interpreted as the Atom feed of a same-named note, leaving asset links broken when no such feed-enabled note existed. The missing-link page now also tailors its "you may create…" hint to the URL extension instead of always suggesting `<url>.md` / `<url>.org` (closes [#547](https://github.com/srid/emanote/issues/547))
- Resolve relative URLs inside `<dir>/index.md` against `<dir>/` instead of its parent ([#651](https://github.com/srid/emanote/pull/651), closes [#608](https://github.com/srid/emanote/issues/608))
- Raw HTML blocks containing a literal `</div>` no longer crash the renderer with `div cannot contain text looking like its end tag` (closes [#119](https://github.com/srid/emanote/issues/119)). Fixed upstream in [srid/heist-extra#13](https://github.com/srid/heist-extra/pull/13) by switching the raw-HTML wrapper to a unique `<rawhtml>` element with `display: contents`.
- A malformed `*.yaml` file (e.g. a non-string mapping key like `[]: foo`) no longer takes the live server down with `BadInput "NonStringKey []"`. The parse error is folded into `SData` itself and surfaced as a banner on the notes whose meta cascade actually depends on the bad file — a broken `subfolder/index.yaml` shows up under `/subfolder/*`, not on every page (closes [#285](https://github.com/srid/emanote/issues/285)).
- TOC sidebar: tightened entry padding and styled the overflow scrollbar (Firefox `scrollbar-width: thin` + WebKit pseudo-element) so long tables of contents no longer surface the chunky OS-default bar (closes [#668](https://github.com/srid/emanote/issues/668)).
- Live server: assets bundled under `_emanote-static/` (skylighting CSS, self-hosted fonts, inverted-tree CSS, emanote-logo, Stork CSS+JS) now cache-bust with `?t=<mtime>` instead of being served bare. Edits to any of these files in `emanote run` invalidate the browser cache without a manual restart. Templates use a new `<emanoteStaticUrl path="…">${url}</emanoteStaticUrl>` splice; the older `${ema:emanoteStaticLayerUrl}` continues to work for third-party templates but skips the cache buster (closes [#666](https://github.com/srid/emanote/issues/666)).

**Developer changes**

- Split the Haskell codebase into acyclic Cabal packages under `packages/`: `emanote-route`, `emanote-source`, `emanote-pandoc`, `emanote-model`, and the app package `emanote` ([#600](https://github.com/srid/emanote/issues/600)).

## 1.4.0.0 (2025-08-18)

**Notable features**

- LLM optimized **single-file Markdown export** ([\#598](https://github.com/srid/emanote/pull/598))
- Obsidian-style **callouts** ([\#466](https://github.com/srid/emanote/pull/466))
- **TOC sidebar** ([\#504](https://github.com/srid/emanote/pull/504))
- Native support for **combining multiple notebooks**
  - Resolve ambiguities based on closer common ancestor ([\#498](https://github.com/srid/emanote/pull/498))
  - Support for folder "index.md" notes ([\#512](https://github.com/srid/emanote/pull/512))
    - Instead of "foo/qux.md", you can now create "foo/qux/index.md"
  - Layers can be mounted in sub-directories, enabling composition of distinct notebooks ([\#523](https://github.com/srid/emanote/pull/523))

**Other improvements**

- Add query syntax for listing folgezetten children & parents ([\#476](https://github.com/srid/emanote/pull/476))
- `emanote run --no-ws` option to disable WebSocket monitoring. This is useful for using Emanote to serve the HTML site directly on the internet, without needing to statically generate it.
- Allow specifying custom page title in sidebar ([\#488](https://github.com/srid/emanote/pull/488))
- Allow specifying `lang` attribute for HTML page in YAML config ([\#485](https://github.com/srid/emanote/pull/485))
- KaTeX support ([\#489](https://github.com/srid/emanote/pull/489))
- Lua filters: filter paths will now be looked up in all layers now.
- Live server now uses Tailwind 3 ([\#503](https://github.com/srid/emanote/pull/503))
- Enable auto identifier for org files ([\#502](https://github.com/srid/emanote/pull/502))
- Support date metadata from the filename when it begins with YYYY-MM-DD ([\#552](https://github.com/srid/emanote/pull/552)).
- Daily notes automatically get a hierarchical tag.
- HTML
  - Support for `<kbd>` rendering https://github.com/srid/heist-extra/pull/8
  - prevent the external link icon from wrapping ([\#528](https://github.com/srid/emanote/pull/528))
  - [live server] Update Tailwind CDN to 3.4.16
- Update ema (2.7.2)
- Nix flake module
  - Add link checker based on `html-proofer`
  - Replace `baseUrl` and `prettyUrls` with `extraConfig`
- **BACKWARDS INCOMPTABILE** changes
  - Removed `ema:homeUrl` (use `baseUrl` instead)
  - `feed.siteUrl` is now `page.siteUrl`
  - A new HTML template layout "default" (unifies and) replaces both "book" and "note" layout. ([\#483](https://github.com/srid/emanote/pull/483))
    - Sidebar tree is now computed from the folgezettel graph, which is a superset of the folder hierarchy. The index page's tree inherits the same.
    - The semantics of `folder-folgezettel` is now applied in inverse (see docs)
  - Add anchor links to headings ([\#500](https://github.com/srid/emanote/pull/500))

**Bug fixes**

- Emanote no longer crashes when run on an empty directory ([\#487](https://github.com/srid/emanote/issues/487))
- Stork search fixes
  - Fix empty stork index generation when using more than 1 layer ([\#493](https://github.com/srid/emanote/issues/493))
  - Stork search index is now uses note path from their associated layer ([\#495](https://github.com/srid/emanote/pull/495))

**Performance improvements**

- Browser-side performance improvement using `idiomorph` ([\#567](https://github.com/srid/emanote/pull/567))
- Fix memory leak and performance gradation overtime in the browser due to Tailwind when using live server ([\#569](https://github.com/srid/emanote/pull/569))

## 1.2.0.0 (2023-08-24)

- Bug fixes
  - Make Stork recognize orgmode notes ([\#413](https://github.com/srid/emanote/issues/413))
- Features
  - Audio embedding ([\#418](https://github.com/srid/emanote/pull/418))
  - Source code embedding ([\#444](https://github.com/srid/emanote/pull/444))
  - Atom feed ([\#451](https://github.com/srid/emanote/pull/451))
  - Add option to show folders first in the sidebar [\#399](https://github.com/srid/emanote/pull/399)
- UI
  - Remove inline styling of H2 elements
- Misc
  - Update Ema to simplify the live server websocket observation logic
  - Use GHC 9.4 & Pandoc 3

## 1.0.2.0 (2023-01-29)

- Nix
  - Reduce Emanote's Nix runtime closure size
- UI
  - Add source map for Stork [\#391](https://github.com/srid/emanote/pull/391)
  - Workaround for Prism.js and Tailwind CSS both using `table` class [\#320](https://github.com/srid/emanote/pull/396)
  - Add option to include YAML frontmatter in the Stork index [\#398](https://github.com/srid/emanote/pull/398)
- Features
  - Timeline backlinks recognize flexible daily notes suffixed with arbitrary string [\#395](https://github.com/srid/emanote/issues/395)
- Performance
  - Address client-side memory leak due to Stork search in live server [\#411](https://github.com/srid/emanote/issues/411#issuecomment-1402056235)
- Misc
  - Ignore toplevel `flake.{nix,lock}` by default.
  - Remove deprecated `_emanote-bin/compile-css` script

## 1.0.0.0 (2022-12-04)

- UI
  - Index pages are no longer marked as 'experimental'
  - Add external link icon to external links (this behaviour is customizable). [\#189](https://github.com/srid/emanote/pull/189)
  - `js.mermaid` snippet uses the "module" script tag approach, which is added to end of `<body>`. See [here](https://mermaid-js.github.io/mermaid/#/n00b-gettingStarted?id=_3-calling-the-javascript-api).
  - Align dates in timeline queries
- Configuration
  - Add `page.bodyHtml` option to inject custom HTML at the end of `<body>` tag.
- Packaging
  - Move tests to their own cabal component (thereby, also, unexpose them from library exposed modules)
  - Wikilink parser is now a separate library: https://github.com/srid/commonmark-wikilink
  - Use `heist-extra` 0.2.0.0 which switches over from `heist-emanote` to using official `heist` package.
- Bug fixes
  - #380 & #386: Better handling of `.org` and `.md` ambiguties. Default layer's `index.md` has been removed.

## 0.8.0.0 (2022-11-03)

Initial release.
