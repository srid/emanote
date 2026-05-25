# /code-police — project rules

Project-specific additions to the built-in rule set in `.claude/skills/code-police/SKILL.md`. Treat each rule below as an extra row in Pass 1's checklist.

The patterns here came out of [#672](https://github.com/srid/emanote/pull/672) (Stork ES-module migration), where seven follow-up commits — three from `/code-police`, four from `/hickey` + `/lowy` — landed on top of the primary feature. Capture: same shape of mistake, same kind of fix, made into a recurring check.

## emanote-docs-single-segment-slug

A docs-page `slug:` must be a **single path segment**, not a multi-segment slash-path. Two reasons:

- The page's directory location already encodes hierarchy. A slug that re-encodes that hierarchy is duplicated state that drifts the moment one of the two moves (a page at `docs/guide/lua-filters/diagrams.md` with `slug: lua-filters/diagrams` carries the same path in two places — a rename of the parent or the page silently breaks the mirror).
- Public URLs stay short and rename-stable. `[[diagrams]]` survives a future reshuffle of where the page sits in the docs tree; `[[lua-filters/diagrams]]` does not.

Acceptable: `slug: diagrams`, `slug: yaml-config`, `slug: html-template`, `slug: i18n.fr` (single segment; dot-namespaced for locale or close variants).

Unacceptable: `slug: lua-filters/diagrams`, `slug: guide/whatever` — anything containing `/`.

Wikilinks that reference the page use the single-segment form too: `[[diagrams]]`, not `[[lua-filters/diagrams]]`.

> _New rule from this PR_: an early draft of [[diagrams]] used `slug: lua-filters/diagrams` to mirror its directory placement under `docs/guide/lua-filters/`. The slug was reduced to `diagrams` so a future move of the file doesn't invalidate every inbound wikilink.

## docs-internal-wikilinks

When reviewing documentation changes under `docs/`, check internal documentation references for wikilinks.

- Use wikilinks for references to existing docs pages, such as `[[yaml-config]]`, `[[html-template]]`, `[[wikilinks]]`, `[[layer]]`, `[[markdown]]`, `[[query]]`, `[[search]]`, and related guide pages.
- Prefer wikilinks over raw relative Markdown links for internal docs references.
- Replace bare mentions of concepts covered by existing docs pages with wikilinks, unless doing so would make the sentence noisy or misleading.
- New or newly promoted guide pages should be linked from the nearest relevant index page, usually `docs/guide.md`.

This rule is meant to catch isolated docs pages that repeat or mention existing Emanote concepts without connecting to the surrounding guide.

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

## emanote-non-obvious-needs-why

A separate audit pass for the built-in `comments-why-not-what` rule, applied to *every* non-trivial chunk of the diff after the structural and fact-check fixes have settled. Without this pass, "missing comment" findings get bundled into the wrong commit (a hickey/lowy refactor commit picks them up incidentally) or — more often — never surface because the reviewer is looking at structure, not legibility.

Walk the diff a final time and ask, file by file, function by function:

- Is there a literal value, branch, or short-circuit whose **why** isn't recoverable from the surrounding code? (Boolean parameters whose polarity isn't named at the call site, default fallbacks that aren't obvious safe-defaults, special cases for edge inputs.)
- Is there a **non-obvious lossy conversion** that a reader might assume is round-trip safe? (Stringifying numbers, dropping unicode, truncating timestamps.)
- Is there an **incomplete defence** that relies on a second mechanism elsewhere? (Static checks that miss dynamic forms; one of two layers of validation.)
- Does a function **interact with another module's invariants** in a way that isn't obvious from this module alone? (Why we extract a writer log here; why we re-wrap a Pandoc Meta; why this branch can never fire.)

For each answer-yes, write the smallest comment that lets a reader pick it up cold — one or two sentences max. Don't comment the **what** the names already convey; don't add docstrings to leaf utilities just to fill space.

> _From [#731](https://github.com/srid/emanote/pull/731)_: after the hickey/lowy and police passes landed, four spots still required guessing — `bannedUses`'s static-vs-runtime split (only catches direct `pandoc.X` access, not aliases), `aesonToPandocMetaValue`'s lossy `Number → MetaString` conversion, the `runWriterT` extraction in `renderLmlHtml` (why diagnostics are captured to render inline *and* abort static builds), and `failOnStaticRenderFilterErrors`'s unnamed `Bool` polarity. None were caught by the structural review because each individual line is structurally fine.

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
