// Adapter between site behaviors and Ema's live-server DOM patches.
//
// Ema patches the DOM via idiomorph on source changes. Behaviors that scan
// the document at startup (e.g. attaching a button to every <pre>) miss the
// elements that arrive in a subsequent patch. `onElement` covers both: it
// runs the callback for matching elements present now and for any inserted
// later. The dataset sentinel guards against re-running on an element that
// idiomorph preserved by identity.
//
// Behaviors that do their own lazy resolution (event delegation +
// isConnected checks, like footnote-popup.js) only need `ready`.

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
