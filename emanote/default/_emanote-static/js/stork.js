// Stork search controller. Owns the modal lifecycle, the WASM index
// load, the Ctrl+K / Esc keyboard shortcuts, and the dark-mode rail
// mirror that re-skins the dialog when the theme toggle flips.
//
// Vendor stork.js (the WASM loader that defines `window.stork`) is
// loaded as a non-module <script src> in stork-search-head.tpl —
// blocking, so it parses before this module evaluates and `window.stork`
// is reliably defined at our top level.
//
// Morph-survival uses three of the four strategies documented in
// morph.js: keydown / click / mutation listeners are attached once on
// document and document.documentElement (both never replaced by morph);
// the WASM init runs once at module load (memory-leak-bound — see
// issue #411); index staleness is signaled by onMorph each time Ema
// swaps in a new page so the next user search refreshes the index.

import { onMorph } from '@emanote/morph';

// Fail fast if the vendor WASM loader didn't run before this module —
// otherwise the first user click would throw a cryptic
// "stork is not defined" instead of pointing at the load-order bug.
// The vendor <script src> is in stork-search-head.tpl; if a future
// template tweak reorders or removes it, this guard names the cause.
if (typeof stork === 'undefined') {
  throw new Error(
    'stork.js (vendor WASM loader at _emanote-static/stork/stork.js) ' +
      'must load before this module. Check stork-search-head.tpl: the ' +
      "vendor <script src> needs to stay in <head>, before base.tpl's " +
      '<emanoteJsBundle /> splice.',
  );
}

// `${value:baseUrl}` (the same string the inline IIFE used to read
// from a marker `<script id="emanote-stork" data-emanote-base-url="…">`)
// is already in the document as `<base href="…">` in <head>.
// document.baseURI gives the resolved absolute form; pathname gives the
// site-relative root that stork.register / stork.initialize expect.
function getBaseUrl() {
  try {
    return new URL(document.baseURI).pathname;
  } catch (e) {
    // URL constructor only throws on unparseable input — base.tpl
    // unconditionally renders <base href="${value:baseUrl}">, so this
    // path is unreachable in practice. If it ever fires, search will
    // fetch the index and WASM from "/" which may be wrong; surface
    // the misconfig rather than silently degrade.
    console.warn(
      '[emanote] stork: document.baseURI is unparseable; ' +
        'falling back to "/" for index + WASM fetches:',
      e,
    );
    return '/';
  }
}

let indexIsStale = false;

// Body class set when the search modal is open; defined in
// components/stork/stork-search-head.tpl's <style> block. Its presence
// IS the modal-open state — see isSearchShown.
const MODAL_HIDDEN_CLASS = 'stork-overflow-hidden-important';

// Deriving the open-state from the DOM (rather than mirroring it in a
// module-scoped flag) avoids the drift hazard when any other code
// mutates the class — the keydown handler reads this on every Esc
// press, so a stale flag would silently no-op.
function isSearchShown() {
  return document.body.classList.contains(MODAL_HIDDEN_CLASS);
}

function registerIndex(forceOverwrite = false) {
  const indexName = 'emanote-search'; // matches input[data-stork] in stork-search.tpl
  stork.register(
    indexName,
    getBaseUrl() + '-/stork.st',
    forceOverwrite ? { forceOverwrite: true } : undefined,
  );
}

function refreshIndex() {
  if (!indexIsStale) return;
  console.log('stork: Reloading index');
  indexIsStale = false;
  // NOTE: This will leak memory; per-morph re-registration is a known
  // trade-off vs. fully reloading the page. See issue #411.
  registerIndex(true);
}

function toggleSearch() {
  refreshIndex();
  const container = document.getElementById('stork-search-container');
  if (!container) return;
  container.classList.toggle('hidden');
  const nowShown = document.body.classList.toggle(MODAL_HIDDEN_CLASS);
  if (nowShown) {
    document.getElementById('stork-search-input')?.focus();
  }
}

function clearSearch() {
  document.getElementById('stork-search-container')?.classList.add('hidden');
  document.body.classList.remove(MODAL_HIDDEN_CLASS);
}

// The stork-wrapper element is rendered into every page's body by
// components/stork/stork-search.tpl, so `getElementById` re-resolves
// fresh each time the observer fires — no stale ref across morph nav.
function applyStorkTheme() {
  const wrapper = document.getElementById('stork-wrapper');
  if (!wrapper) return;
  const isDark = document.documentElement.classList.contains('dark');
  wrapper.classList.remove('stork-wrapper-edible', 'stork-wrapper-edible-dark');
  wrapper.classList.add(isDark ? 'stork-wrapper-edible-dark' : 'stork-wrapper-edible');
}

// --- one-shot setup at module load (runs once per session) ---

// Initialize WASM on window-load (the only time stork.initialize is
// safe to call) and register the initial index. Done once; later morph
// navs only mark the index stale, deferring re-register until the
// user actually opens search.
if (document.readyState === 'complete') {
  stork.initialize(getBaseUrl() + '_emanote-static/stork/stork.wasm');
  registerIndex();
} else {
  window.addEventListener('load', () => {
    stork.initialize(getBaseUrl() + '_emanote-static/stork/stork.wasm');
    registerIndex();
  });
}

// Single delegated click listener for the toggle attribute. Buttons in
// the breadcrumbs / sidebar / minimal-layout header AND the modal
// backdrop all carry data-emanote-stork-toggle, so one handler covers
// open AND close (the backdrop is only visible while the modal is
// open, so toggling closes it).
document.addEventListener('click', (e) => {
  if (e.target.closest('[data-emanote-stork-toggle]')) {
    toggleSearch();
  }
});

// Ctrl+K / Cmd+K opens; Esc closes when modal is open.
document.addEventListener('keydown', (e) => {
  if (isSearchShown() && e.key === 'Escape') {
    clearSearch();
    e.preventDefault();
  } else if ((e.key === 'k' || e.key === 'K') && (e.ctrlKey || e.metaKey)) {
    toggleSearch();
    e.preventDefault();
  }
});

// Mirror the .dark class from <html> onto #stork-wrapper so the search
// dialog re-skins live when the theme toggle flips. documentElement is
// never replaced by morph, so the observer attaches once.
new MutationObserver(applyStorkTheme).observe(document.documentElement, {
  attributes: true,
  attributeFilter: ['class'],
});
applyStorkTheme();

// Each in-app morph nav means a new note's content is now showing;
// the cached index doesn't reflect the new corpus. Mark it stale so
// the next user search re-registers (lazy, on-demand). Same trade-off
// the inline IIFE made before this module existed.
//
// Also re-apply the dark/light wrapper class: the morph swaps in a
// fresh #stork-wrapper element from the new page's body. The
// MutationObserver above only fires on <html>.class changes — if the
// user navigates without toggling the theme, the new wrapper would
// stay un-classed and the search dialog would render unstyled (just
// the bare <input> + <output>, no edible.css rules applying). Fold
// both concerns into one onMorph callback.
onMorph(() => {
  console.log('stork: Marking index as stale');
  indexIsStale = true;
  applyStorkTheme();
});
