# /code-police — project rules

Project-specific additions to the built-in rule set in `.claude/skills/code-police/SKILL.md`. Treat each rule below as an extra row in Pass 1's checklist.

The patterns here came out of [#672](https://github.com/srid/emanote/pull/672) (Stork ES-module migration), where seven follow-up commits — three from `/code-police`, four from `/hickey` + `/lowy` — landed on top of the primary feature. Capture: same shape of mistake, same kind of fix, made into a recurring check.

## emanote-tailwind-first

When styling Heist templates or JS-generated DOM, prefer **inline Tailwind utility classes** over CSS rules in `styles.tpl`. The grandfathered CSS in that file lives there because the rule it expresses doesn't fit the utility-class shape (e.g. complex `@keyframes`, `:popover-open` pseudo-states, or selectors that target Pandoc-emitted HTML with no class hook). Everything else belongs as `class="…"` in the template.

A new CSS rule in `styles.tpl` is only acceptable when:

- The selector targets an element Pandoc / a vendor library emits without classes (`main table`, `pre`, `kbd`, `:popover-open` modifiers). Add a one-line comment explaining the no-class-hook constraint.
- The rule needs `@keyframes` or other at-rule constructs Tailwind doesn't express inline.
- The same property repeats across a CSS-property family that Tailwind would force you to spell as one arbitrary value per case (e.g. complex `font-feature-settings` runs).

If you find yourself writing `.foo { color: …; padding: …; }` in CSS where `<div class="text-… p-…">` would do the job, the template is the right home — utility classes are scanned by Tailwind's JIT and tree-shaken with the rest of the build, while raw CSS in `styles.tpl` ships unconditionally.

> _Rationale_: keeping styling in templates means a reader reviewing one component sees its full styling at the call site rather than having to cross-reference a separate `styles.tpl` `data-category` block. It also makes the surface visible to `/code-police`'s rule-of-three reviews — three sites of the same Tailwind class string become a candidate for `<bind>` or extraction; three CSS rules in `styles.tpl` are invisible.

## emanote-no-one-field-options-bag

A function that takes a single-key options object — `fn(x, { onlyKey: value })` — is an options-bag larp dressed as future-proofing. Replace with a positional parameter (`fn(x, onlyKey = default)`). Threshold: **if at most one of N call sites actually passes anything, the bag has no users to justify it.**

Future-proofing argument ("we might add more keys later") is rejected — add the bag *when* the second key arrives, not before.

> _From #672_: `registerIndex({ forceOverwrite: true })` had two callers; only one passed anything → replaced with `registerIndex(forceOverwrite = false)`.

## emanote-dom-over-mirror

When UI state is already represented in the DOM (a body class, an `aria-*` attribute, an element's presence), **query the DOM** rather than maintaining a parallel JS variable that "should track" it. Mirror flags drift when anything outside the owning module mutates the DOM (devtools, utility scripts, future code), leaving handlers reacting to stale state — usually as a silent no-op that's invisible in review.

The `classList.contains` cost on a hot path is irrelevant; correctness wins. Cache only if a profile shows a real bottleneck.

> _From #672_: `searchShown` mirrored `body.stork-overflow-hidden-important`. Esc-handler silently no-op'd if the class was toggled by anything outside the module. Replaced with `isSearchShown()` querying the class directly.

## emanote-vendor-global-guard

An ES module that depends on a vendor global defined by a separate `<script src>` (CDN, inline `<script>` in a template, etc.) **must guard at module evaluation**:

```js
if (typeof vendorGlobal === 'undefined') {
  throw new Error(`vendorGlobal is missing — expected from <some-template>.tpl`);
}
```

Without the guard, a future template tweak that reorders or removes the dependency surfaces as a cryptic `X is not defined` on the user's first interaction — far from the actual cause. The error message must name the template/script that should provide it, so the failure mode is self-documenting.

> _From #672_: `stork.js` assumed `window.stork` from `stork-search-head.tpl`'s vendor `<script src>`. Added a guard pointing at that template.

## Calibration for built-in rules

These are not new rules — they're concrete examples for built-in rules that come up often in this codebase. Use them when judging borderline cases.

### `no-silent-error-swallowing` — URL/parse failures must log

The bare `catch {}` form is already disallowed, but a *named* catch that returns a fallback value is just as silent if it doesn't log. URL parsing, `JSON.parse`, attribute decoding — these all have valid fallbacks (root path, empty object, "off") *and* a real diagnostic story (`console.warn` with the offending input + the consequence). Both are required.

> _From #672_: `getBaseUrl()` caught a malformed `document.baseURI` and degraded to `/` — silently. Fix: `console.warn` with the bad URL + "search path resolution falling back to /".

### `dry-rule-of-three` — string literals across hot-path methods

The threshold is three; `'stork-overflow-hidden-important'` appearing across `toggleSearch` / `clearSearch` / `isSearchShown` is exactly that case. Extract to a `const MODAL_HIDDEN_CLASS = '...';` at module top so a typo in one of the three sites can't silently desync the open-state machinery.
