# Revision history for emanote

## 2.0.0.0 (Unreleased)

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
  - **Sidebar month calendar** ([#700](https://github.com/srid/emanote/issues/700)): when a sidebar tree node holds nothing but daily-note leaves of one month (e.g. `Daily/2026/04/2026-04-21.md` … `2026-04-30.md`), the children list is replaced in-place by a 7-column calendar grid for that month. Cells with notes link to the daily note; missing days render as muted day numbers. Date detection lives in Haskell (`Calendar.parseRouteDay` emits `data-iso-date` on each leaf via the new `node:iso-date` splice) so JS never re-implements the YYYY-MM-DD parser. Cell palette + size constants extracted from `timeline-heatmap.js` into a shared `calendar-grid` module so a Tailwind palette refresh edits one file.
  - **Default theme refresh** ([#699](https://github.com/srid/emanote/pull/699)): every panel — title, sidebar, right-panel (TOC + backlinks + timeline), bottom strip, footer — attaches inside one rounded `#container` card, so the page UI reads as a single composed unit instead of stacked stripes. Wikilinks, backlinks, queries, timeline entries, tasks index, and inline tags share one chip palette (`bg-primary-50/70 text-primary-600 font-semibold tracking-tight`); TOC drops primary entirely (entries map to plain-text headings). Heading scale tightens to a ~1.20 ratio with uniform `font-semibold`. Daily-note backlinks render as a year-stacked heatmap with cell-hover context flyouts. Special pages (`/-/all`, `/-/tags`, `/-/tasks`) pick up the same chrome plus a back-to-Home link. Tables, in-prose task lists, and the footer all get coordinated styling so the page reads as one design language. Mona Sans replaces Space Grotesk for the chrome typeface.
- Mermaid: add `elk` layout ([#618](https://github.com/srid/emanote/pull/618))
- Home Manager module: macOS support via launchd ([#623](https://github.com/srid/emanote/pull/623))

**Bug fixes**

- Timeline heatmap backlinks now use the daily note's route-derived date instead of parsing `YYYY-MM-DD` out of the rendered title, so custom-titled daily notes still appear in the heatmap.
- Cascade-declared `tags` no longer disappear from a child note that declares any of its own. The metadata-cascade merger previously inherited `aeson-extra`'s `lodashMerge`, which aligns arrays by index — `tags: [team-doc]` in `folder.yaml` plus `tags: [internal-note]` in `folder/note.md` produced `[internal-note]`, dropping the cascaded entry both from the per-page chip strip and from the `#352` global tag index. The merger now unions cascade arrays; see [yaml-config](docs/guide/yaml-config.md) for the full contract. `aeson-extra` is no longer a dependency (closes [#697](https://github.com/srid/emanote/issues/697)).
- Wiki link custom titles now render HTML entities like `&nbsp;` the same way regular Markdown link labels do. Previously `[[note|Spivak&nbsp;(2014)]]` rendered the entity text literally as `&nbsp;` (closes [#441](https://github.com/srid/emanote/issues/441)).
- Atom feed: a feed query that matches no notes no longer crashes the build; an empty-but-valid Atom document is emitted instead. Configuration errors (missing/invalid query block, missing `page.siteUrl`) still fail loudly ([#490](https://github.com/srid/emanote/issues/490), [#650](https://github.com/srid/emanote/pull/650))
- Markdown links to a static `.xml` asset (e.g. `[Test](./test.xml)`) now resolve to the file. Previously a `.xml` URL was always interpreted as the Atom feed of a same-named note, leaving asset links broken when no such feed-enabled note existed. The missing-link page now also tailors its "you may create…" hint to the URL extension instead of always suggesting `<url>.md` / `<url>.org` (closes [#547](https://github.com/srid/emanote/issues/547))
- Resolve relative URLs inside `<dir>/index.md` against `<dir>/` instead of its parent ([#651](https://github.com/srid/emanote/pull/651), closes [#608](https://github.com/srid/emanote/issues/608))
- Raw HTML blocks containing a literal `</div>` no longer crash the renderer with `div cannot contain text looking like its end tag` (closes [#119](https://github.com/srid/emanote/issues/119)). Fixed upstream in [srid/heist-extra#13](https://github.com/srid/heist-extra/pull/13) by switching the raw-HTML wrapper to a unique `<rawhtml>` element with `display: contents`.
- Raw HTML blocks separated by blank lines (CommonMark "type 6", e.g. `<details>` … markdown … `</details>`) now nest the markdown content as a real DOM child of the surrounding element instead of leaving the open and close tags trapped inside their own `<rawhtml>` wrappers. Fixed upstream by the new `groupRawHtmlBlocks` AST pass in heist-extra (closes [#433](https://github.com/srid/emanote/issues/433)).
- A malformed `*.yaml` file (e.g. a non-string mapping key like `[]: foo`) no longer takes the live server down with `BadInput "NonStringKey []"`. The parse error is folded into `SData` itself and surfaced as a banner on the notes whose meta cascade actually depends on the bad file — a broken `subfolder/index.yaml` shows up under `/subfolder/*`, not on every page (closes [#285](https://github.com/srid/emanote/issues/285)).
- TOC sidebar: tightened entry padding and styled the overflow scrollbar (Firefox `scrollbar-width: thin` + WebKit pseudo-element) so long tables of contents no longer surface the chunky OS-default bar (closes [#668](https://github.com/srid/emanote/issues/668)).
- Markdown tables now honour Pandoc's column alignment, column widths, cell `rowspan` / `colspan`, row & cell attributes, and table footers — previously every field beyond "rows of cells" was discarded (closes [#27](https://github.com/srid/emanote/issues/27); fixed upstream in [srid/heist-extra#15](https://github.com/srid/heist-extra/pull/15)).
- Live server: assets bundled under `_emanote-static/` (skylighting CSS, self-hosted fonts, inverted-tree CSS, emanote-logo, Stork CSS+JS) now cache-bust with `?t=<mtime>` instead of being served bare. Edits to any of these files in `emanote run` invalidate the browser cache without a manual restart. Templates use a new `<emanoteStaticUrl path="…">${url}</emanoteStaticUrl>` splice; the older `${ema:emanoteStaticLayerUrl}` continues to work for third-party templates but skips the cache buster (closes [#666](https://github.com/srid/emanote/issues/666)).
- A Markdown link whose label contains a URL or email address now renders as a single hyperlink instead of being split into the parent link plus separate autolinks for the embedded URLs. `commonmark-hs`'s autolink extension produces nested `Link` nodes for these labels (including positions wrapped in emphasis); a new `flattenNestedLinks` pass — added as the last step of `Emanote.Pandoc.BuiltinFilters.preparePandoc` — unwraps the inner `Link` so the displayed label survives but its target is dropped (closes [#349](https://github.com/srid/emanote/issues/349)).
- Home Manager module: `services.emanote.package` now defaults to Emanote's own flake package when importing `emanote.homeManagerModule`, so users no longer need to set the package option just to avoid a missing `pkgs.emanote` attribute (closes [#309](https://github.com/srid/emanote/issues/309)).
- Backlinks: when one note links to a target several times, the individual context cards now appear in source order — the order they show up in the source `.md` file. Previously they were sorted lexicographically by the surrounding block content, and two links that shared an identical context (e.g. `[[Foo]] and [[Foo]]` in one paragraph) were collapsed into one card by the relation-index dedup. `Rel` now carries a `_relSrcPos` pandoc-traversal index that breaks ties between rels sharing `(_relFrom, _relTo)` so source order survives `IxSet.toList` and the `groupNE` aggregation (closes [#186](https://github.com/srid/emanote/issues/186)).
- Folders named `index` no longer collapse breadcrumb / sidebar links to a higher ancestor. A note like `index/index/index/example.md` previously emitted `/index/index/` for its direct-parent breadcrumb because the folder placeholder for `index/index/index/` (LML route `("index","index","index")`) encoded to `index/index/index.html` and Ema's pretty URL stripped the trailing `index` segment. `noteHtmlRoute` now appends a trailing `index` slug whenever the LML route already ends in `index` (and isn't the lone root `index`), so the encoded HTML path becomes `index/index/index/index.html` and the URL resolves to `/index/index/index/` as expected. The same fix applies to feed (`*.xml`) URLs of index files inside index folders, which were similarly truncated. Closes [#542](https://github.com/srid/emanote/issues/542).
- Cyclic note embeds (`![[a]]` in `b.md` together with `![[b]]` in `a.md`, or a self-embed) now render an inline `↺ Cyclic embed: <title> (via …)` placeholder — naming the offending note plus the chain that closed the loop — instead of expanding the embed without a fixpoint and either hanging the live preview or producing arbitrarily deep nested output. Internally, the embed-ancestor stack is stashed in `RenderCtx`'s typed user-data slot (added upstream in [srid/heist-extra#17](https://github.com/srid/heist-extra/pull/17)) so only the embed renderer reads it; URL / callout / query renderers stop seeing the embed-only concern in their signatures. On a hit the renderer falls back to a `.emanote\:error\:cyclic-embed` div (closes [#362](https://github.com/srid/emanote/issues/362), [#684](https://github.com/srid/emanote/issues/684)).
- Tag links now percent-encode characters reserved by RFC 3986, so tags containing `#` (e.g. Zettelkasten "structure note" tags like `##§1`) or non-ASCII characters produce a clickable URL instead of one the browser truncates at the literal `#` and treats as an HTML fragment. Both inline `#tag` syntax (rewritten by `Emanote.Pandoc.BuiltinFilters`) and the page-metadata tag chips in the default `metadata.tpl` route through the same encoding path used elsewhere for tag-index links; the template now drives a new `ema:tagsList` splice with per-tag `${ema:tag:url}` instead of pasting the raw tag string into the `href` (closes [#199](https://github.com/srid/emanote/issues/199)).
- Tags inherited from a sibling YAML cascade (`subfolder.yaml` declaring `tags: [foo]`, or any `*.yaml` above the note in the route hierarchy) now reach the global tag index, the per-tag page (`/-/tags/foo.html`), and `tag:#foo` queries. Previously they rendered on each child note's metadata chip-strip but were invisible to anything that walked the tag axis: the IxNote tag index was a pure `Note -> [Tag]` and so could only see the note's own frontmatter, never the cascade. `modelTags` and the tag-by-tag query path now consult the same effective metadata used for per-page rendering, so the global index agrees with what each page already displays (closes [#352](https://github.com/srid/emanote/issues/352)).

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
