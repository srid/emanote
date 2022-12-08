# Revision history for emanote

## Unreleased

- Nix
  - Reduce Emanote's Nix runtime closure size
- UI
  - Add source map for Stork [\#391](https://github.com/EmaApps/emanote/pull/391)
- New features
  - Ignore toplevel `flake.{nix,lock}` by default.

## 1.0.0.0 (2022-12-04)

- UI
  - Index pages are no longer marked as 'experimental'
  - Add external link icon to external links (this behaviour is customizable). [\#189](https://github.com/EmaApps/emanote/pull/189)
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
