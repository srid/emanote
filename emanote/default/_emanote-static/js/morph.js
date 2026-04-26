// Adapter between site behaviors and Ema's live-server DOM patches.
//
// Ema patches the DOM via idiomorph on source changes / in-app navigation.
// A behavior must choose ONE strategy for surviving morph; pick the one
// that matches its data dependencies:
//
//   1. STATELESS GLOBAL — the behavior is just a function attached to
//      `window.emanote.*` (e.g. theme-toggle). The morphed-in DOM still
//      calls it via `onclick` attrs. No setup, no state, no choice to
//      make. Use this when there's nothing to re-init.
//
//   2. EVENT DELEGATION + LAZY RESOLUTION — single listener on `document`,
//      re-resolves DOM refs on each event with `isConnected` checks
//      (e.g. footnote-popup). Survives morph because the listener target
//      (document) is never replaced and lookups are deferred to event
//      time. Use this when the behavior only acts on user interaction.
//
//   3. PER-ELEMENT WIRE-ON-APPEAR — `onElement(selector, fn)` (below).
//      Runs `fn` for every matching element present now AND any inserted
//      later (via a single shared MutationObserver). A dataset sentinel
//      prevents double-wiring on idiomorph-preserved elements. Use this
//      when each matching element needs its own attached state (e.g.
//      code-copy attaches a button + click handler per <pre>).
//
//   4. RE-INIT ON MORPH — `onMorph(fn)` (below) subscribes to Ema's
//      post-morph hook. The behavior tears down its own observers/state
//      and re-runs setup against the new DOM. Use this when none of the
//      above fit because the behavior needs to capture DOM refs at
//      setup time (e.g. toc-spy's IntersectionObserver targets specific
//      heading elements that change per page).
//
// Most new behaviors will fit (1), (2), or (3). Reach for (4) only if
// the data dependency is genuinely \"observe these specific elements\"
// rather than \"react to events on these elements\".

const registry = [];
let observerStarted = false;
let nextSentinel = 0;

export function ready(fn) {
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', fn);
  } else {
    fn();
  }
}

// Subscribe to Ema's post-morph hook. Behaviors that capture DOM refs
// at setup time (e.g. toc-spy's IntersectionObserver) need to tear down
// + re-init when Ema swaps in a new page via idiomorph. Wrapping the
// raw event name here keeps it a one-place internal detail of morph.js;
// behavior code shouldn't need to know which Ema event drives this.
export function onMorph(fn) {
  window.addEventListener('EMAHotReload', fn);
}

export function onElement(selector, fn) {
  // Each registration gets its own opaque dataset key. Earlier this was
  // derived from the selector by character-substitution, but that risks
  // collisions on similarly-shaped selectors (e.g. 'sup[data-foo]' and
  // 'sup[data_foo]' hashed to the same string), which would silently
  // skip the second registration. The counter is bounded by the number
  // of behavior registrations (single digits), and the sentinel is
  // purely an idempotence guard — its identity carries no meaning.
  const tag = 'emanoteWired_' + nextSentinel++;
  registry.push({ selector, tag, fn });
  ready(() => {
    scan(document, selector, tag, fn);
    if (!observerStarted) startObserver();
  });
}

function scan(root, selector, tag, fn) {
  // querySelectorAll on a document or element finds descendants only;
  // for added subtrees we also have to test the root itself.
  if (root.nodeType === 1 && root.matches?.(selector)) {
    wire(root, tag, fn);
  }
  root.querySelectorAll?.(selector).forEach((el) => wire(el, tag, fn));
}

function wire(el, tag, fn) {
  if (el.dataset[tag]) return;
  el.dataset[tag] = '1';
  fn(el);
}

function startObserver() {
  observerStarted = true;
  const mo = new MutationObserver((mutations) => {
    for (const m of mutations) {
      m.addedNodes.forEach((node) => {
        if (node.nodeType !== 1) return;
        for (const { selector, tag, fn } of registry) {
          scan(node, selector, tag, fn);
        }
      });
    }
  });
  mo.observe(document.body, { childList: true, subtree: true });
}
