// Entry point for site-authored interactive behaviors. Loaded once from
// base.tpl as <script type="module">, which is defer-by-default so it
// doesn't block first paint. Each behavior module attaches itself on
// import; this file just enumerates them.
//
// Out of scope (keeps its own inline script for now):
//   - Stork search controller (templates/components/stork/stork-search-head.tpl)
//   - FOUC theme applier (base.tpl) — must run pre-paint, can't be deferred

// Bare specifiers (resolved via the importmap that base.tpl renders
// alongside this script tag). Bare names let live-mode cache-busting —
// the importmap remaps each name to the corresponding /js/<name>.js?t=<mtime>
// URL, so any module change triggers a fresh fetch of that module.
// Plain relative imports (./foo.js) wouldn't pick up ?t= at the
// resolution step, so transitive dependencies would stale-cache.
//
// IMPORTANT: the names below MUST agree with `emanoteJsModuleNames` in
// `emanote/src/Emanote/View/JsBundle.hs` (the Haskell-side importmap
// manifest). A name in the importmap that's missing here is dead
// weight; a name imported here that's missing from the importmap
// throws a bare-specifier resolution failure at module load. Both
// lists are short — eyeball them together when adding a behavior.
// Tracked: https://github.com/srid/emanote/issues/669
import '@emanote/theme-toggle';
import '@emanote/code-copy';
import '@emanote/toc-spy';
import '@emanote/footnote-popup';
