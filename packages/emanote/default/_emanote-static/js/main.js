// Entry point for site-authored interactive behaviors. Loaded once from
// base.tpl as <script type="module">, which is defer-by-default so it
// doesn't block first paint.
//
// Single source of truth: the importmap rendered alongside this script
// (built from `emanoteJsModuleNames` in `packages/emanote/src/Emanote/View/JsBundle.hs`).
// We read each bare specifier from it and dynamically import — so adding
// a behavior is a one-line change on the Haskell side, no second list
// to keep in sync. Browser dedupes module-loads by URL; behaviors that
// statically import each other (e.g. `code-copy.js` → `morph.js`) hit
// the same cached instance.
//
// Out of scope (keeps its own inline script for now):
//   - Stork search controller (templates/components/stork/stork-search-head.tpl)
//   - FOUC theme applier (base.tpl) — must run pre-paint, can't be deferred

const importmap = document.querySelector('script[type="importmap"]');
if (importmap) {
  const { imports } = JSON.parse(importmap.textContent);
  await Promise.all(Object.keys(imports).map((name) => import(name)));
}
