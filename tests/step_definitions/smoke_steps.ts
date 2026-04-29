import { Given, When, Then } from "@cucumber/cucumber";
import { EmanoteWorld } from "../support/world.ts";
import { morphNav, openRoute } from "../support/navigation.ts";
import * as assert from "node:assert";

When("I open {string}", async function (this: EmanoteWorld, url: string) {
  await openRoute(this.page, url);
});

When("I fetch {string}", async function (this: EmanoteWorld, url: string) {
  this.lastResponse = await this.page.request.get(url);
});

// #119: a raw HTML block containing a literal `</div>` used to truncate the
// page mid-render with `div cannot contain text looking like its end tag`.
// Asserting on a marker *inside* the raw block proves the page reached past
// the offending node — a partial response wouldn't include the marker.
Then(
  "the page contains an element with data-marker {string}",
  async function (this: EmanoteWorld, marker: string) {
    const count = await this.page
      .locator(`[data-marker="${marker}"]`)
      .count();
    assert.ok(
      count > 0,
      `Expected an element with data-marker="${marker}"; got ${count}. Either the raw-HTML wrapper regressed (xmlhtml's "div cannot contain text…" path) and the page was truncated, or Pandoc no longer recognises the source as a raw HTML block.`,
    );
  },
);

// #433: orphan opener/closer raw-HTML tags around markdown content used to
// produce two stranded `<rawhtml>` wrappers immediately adjacent to the
// `<details>` opener and closer. Browsers' lenient HTML5 parser recovers
// the broken stream into a DOM that nests the marker under <details>, so a
// DOM-level `closest("details")` check passes on master too. Catching the
// regression requires inspecting the *emitted* HTML directly. The
// load-bearing structural difference is the `<rawhtml ...><details>`
// adjacency on master vs. a bare `<details>` on the fix.
Then(
  "the emitted HTML for {string} wraps no <rawhtml> around its <details> tags",
  async function (this: EmanoteWorld, route: string) {
    const response = await this.page.request.get(route);
    assert.ok(response.ok(), `Failed to fetch ${route}: ${response.status()}`);
    const html = await response.text();
    const wrappedOpener = /<rawhtml[^>]*>\s*<details(?:\s|>)/.test(html);
    const wrappedCloser = /<rawhtml[^>]*>\s*<\/details>/.test(html);
    assert.ok(
      !wrappedOpener && !wrappedCloser,
      `Expected ${route} to emit a bare <details>...</details> with no surrounding <rawhtml> wrappers. Got wrappedOpener=${wrappedOpener}, wrappedCloser=${wrappedCloser}. The orphan-RawHtml grouping pass (heist-extra: groupRawHtmlBlocks) likely regressed.`,
    );
  },
);

// #285 — two complementary checks. The "no Ema exception" assertion
// guards the *crash* (the original bug surface); the banner assertion
// guards the *visibility* of the error so a parse failure can't fail
// silently. Both must hold for the regression to be considered fixed.
const EMA_EXCEPTION_MARKERS = [
  "Ema App threw an exception",
  "Unable to render template",
];

Then(
  "the page rendered without an Ema exception",
  async function (this: EmanoteWorld) {
    const text = (await this.page.textContent("body")) ?? "";
    for (const marker of EMA_EXCEPTION_MARKERS) {
      assert.ok(
        !text.includes(marker),
        `Page body contains ${JSON.stringify(marker)} — #285 regressed: a malformed *.yaml file is again crashing the model patch handler. First 200 chars of body: ${JSON.stringify(text.slice(0, 200))}.`,
      );
    }
  },
);

// The banner is a Pandoc Div with class `emanote:error`, which an existing
// Heist splice rewrites into Tailwind utility classes. Match by the unique
// header text instead of the source class — the test shouldn't break the
// next time those utilities get tweaked.
const YAML_BANNER_HEADER = "Emanote: bad YAML files";

Then(
  "the page shows the YAML errors banner",
  async function (this: EmanoteWorld) {
    const header = this.page.getByText(YAML_BANNER_HEADER);
    await header.waitFor({ state: "attached", timeout: 5_000 });
    const text = (await this.page.textContent("body")) ?? "";
    assert.ok(
      text.includes("broken-285.yaml"),
      `Banner header rendered but did not name the broken fixture file — the per-file error message stopped propagating. Got: ${JSON.stringify(text.slice(0, 400))}.`,
    );
  },
);

Then(
  "the page does not show the YAML errors banner",
  async function (this: EmanoteWorld) {
    const text = (await this.page.textContent("body")) ?? "";
    assert.ok(
      !text.includes(YAML_BANNER_HEADER),
      `Banner header leaked onto a page whose route cascade does not include the broken yaml — yaml-error scoping regressed. First 400 chars: ${JSON.stringify(text.slice(0, 400))}.`,
    );
  },
);

Then(
  "the response is a valid Atom feed",
  async function (this: EmanoteWorld) {
    const resp = this.lastResponse;
    assert.ok(resp, "No prior `When I fetch …` recorded a response.");
    assert.strictEqual(
      resp.status(),
      200,
      `Expected 200; got ${resp.status()}. The feed route should serve an empty-but-valid Atom document, not error out (see #490).`,
    );
    const body = await resp.text();
    assert.ok(
      body.startsWith("<?xml"),
      `Expected an XML prolog; first 100 chars: ${JSON.stringify(body.slice(0, 100))}.`,
    );
    assert.ok(
      body.includes("<feed"),
      `Expected a top-level <feed> element; body did not contain one. First 200 chars: ${JSON.stringify(body.slice(0, 200))}.`,
    );
  },
);

Given(
  "I note the resolved primary palette at {string}",
  async function (this: EmanoteWorld, url: string) {
    await this.page.goto(url, { waitUntil: "domcontentloaded" });
    this.notedPrimary500 = await this.waitForPrimaryResolved();
  },
);

Then(
  "the primary palette custom property resolves to a non-empty value",
  async function (this: EmanoteWorld) {
    try {
      await this.waitForPrimaryResolved();
    } catch (e) {
      throw new Error(
        `--color-primary-500 never resolved on :root. The theme-remap alias or the target palette var is missing (see #633). Underlying: ${String(e)}`,
      );
    }
  },
);

// texmath emits `<math xmlns="…" display="…">` for every expression. The HTML
// parser places `<math>` in the MathML namespace automatically, so
// `element.namespaceURI` is the reliable check — the `xmlns=` attribute is a
// namespace declaration that CSS `[xmlns=…]` selectors don't see. We assert
// the `display` attribute via a CSS selector and the namespace via JS.
const MATHML_NS = "http://www.w3.org/1998/Math/MathML";

async function assertMathMLCount(
  page: EmanoteWorld["page"],
  display: "inline" | "block",
  msgSuffix: string,
) {
  const count = await page.evaluate(
    ({ display, ns }) =>
      [...document.querySelectorAll(`math[display="${display}"]`)].filter(
        (el) => el.namespaceURI === ns,
      ).length,
    { display, ns: MATHML_NS },
  );
  assert.ok(
    count > 0,
    `Expected at least one ${display} MathML element; found ${count}. ${msgSuffix}`,
  );
}

Then(
  "the page contains an inline <math> element in the MathML namespace",
  async function (this: EmanoteWorld) {
    await assertMathMLCount(
      this.page,
      "inline",
      "emanote.staticMath default may have regressed, or texmath's InlineMath → display=\"inline\" mapping changed.",
    );
  },
);

Then(
  "the page contains a block <math> element in the MathML namespace",
  async function (this: EmanoteWorld) {
    await assertMathMLCount(
      this.page,
      "block",
      "The $$…$$ display-math path is broken.",
    );
  },
);

Then(
  "no KaTeX stylesheet is referenced",
  async function (this: EmanoteWorld) {
    // The js.katex snippet was removed from the default config in the same PR
    // that flipped staticMath on. Default-config sites should therefore never
    // emit a katex stylesheet link.
    const count = await this.page
      .locator('link[href*="katex"], script[src*="katex"]')
      .count();
    assert.strictEqual(
      count,
      0,
      `Default config should not pull in KaTeX; found ${count} katex asset reference(s). Did the js.katex snippet or a default \`page.headHtml\` leak back in?`,
    );
  },
);

Then(
  "the resolved primary palette differs from the noted value",
  async function (this: EmanoteWorld) {
    assert.ok(
      this.notedPrimary500,
      "Noted primary palette is empty — the Given step did not record a baseline.",
    );
    const value = await this.waitForPrimaryResolved();
    assert.notStrictEqual(
      value,
      this.notedPrimary500,
      `Expected per-page template.theme override to change --color-primary-500; both pages resolved to ${JSON.stringify(value)}.`,
    );
  },
);

// Issue #608: a relative link inside `<dir>/index.md` must resolve against
// `<dir>/`, not the parent of the canonicalized route. Scoped to <article>
// because the sidebar nav also surfaces a child link to the same target —
// and that path is auto-generated from the route hierarchy, so it stays
// correct even when the bug is live. Only the article-body anchor exercises
// the relative-URL resolver we're testing.
Then(
  "the article link with text {string} has href containing {string}",
  async function (this: EmanoteWorld, linkText: string, needle: string) {
    const link = this.page.locator(`article a:has-text("${linkText}")`).first();
    await link.waitFor({ state: "attached", timeout: 5_000 });
    const href = await link.getAttribute("href");
    assert.ok(
      href && href.includes(needle),
      `Article link "${linkText}" expected href to contain ${JSON.stringify(needle)}, got ${JSON.stringify(href)}.`,
    );
  },
);

Then(
  "the first article link has HTML containing {string}",
  async function (this: EmanoteWorld, needle: string) {
    const link = this.page.locator("article a").first();
    await link.waitFor({ state: "attached", timeout: 5_000 });
    const html = await link.innerHTML();
    assert.ok(
      html.includes(needle),
      `First article link expected HTML to contain ${JSON.stringify(needle)}, got ${JSON.stringify(html)}.`,
    );
  },
);

// #349: count assertion is the strongest regression guard. Before the fix,
// each of the four cases produces 2-3 anchors (parent + autolinks); after
// the fix, exactly 4 anchors target the four cases. Filter on the shared
// `issue349` substring (matches both `issue349-caseN.example.com` and the
// mailto form `case2@issue349.example.com`) so any regression that adds
// stray autolinks bumps the count and trips the assertion.
Then(
  "the article body has exactly {int} hyperlinks to issue-349 case targets",
  async function (this: EmanoteWorld, expected: number) {
    const hrefs = await this.page.$$eval("article a[href]", (anchors) =>
      anchors
        .map((a) => a.getAttribute("href") ?? "")
        .filter((h) => h.includes("issue349")),
    );
    assert.strictEqual(
      hrefs.length,
      expected,
      `Expected ${expected} article links to issue349 targets, got ${hrefs.length}: ${JSON.stringify(hrefs)}.`,
    );
  },
);

const POPOVER_SEL = "#emanote-footnote-popover";

When(
  "I click the footnote ref with index {string} in the parent body",
  async function (this: EmanoteWorld, idx: string) {
    // Top-level refs are the ones whose closest [data-footnote-scope] is null.
    // Playwright's `:not(:has(…ancestor))` trick doesn't exist, so filter in-page.
    const handle = await this.page.evaluateHandle((i) => {
      const refs = document.querySelectorAll(
        `sup[data-footnote-ref="${i}"]`,
      );
      for (const r of refs) {
        if (!r.closest("[data-footnote-scope]")) return r as HTMLElement;
      }
      return null;
    }, idx);
    const el = handle.asElement();
    assert.ok(el, `No top-level sup[data-footnote-ref="${idx}"] on the page.`);
    await el.click();
  },
);

When(
  "I click the footnote ref with index {string} inside an embedded note",
  async function (this: EmanoteWorld, idx: string) {
    // Embeds render as <section data-footnote-scope>; callouts as <div>.
    // Match only the section form so this step doesn't pick up a
    // callout-scoped ref by accident.
    const handle = await this.page.evaluateHandle((i) => {
      const refs = document.querySelectorAll(
        `section[data-footnote-scope] sup[data-footnote-ref="${i}"]`,
      );
      return (refs[0] as HTMLElement) ?? null;
    }, idx);
    const el = handle.asElement();
    assert.ok(
      el,
      `No section[data-footnote-scope] sup[data-footnote-ref="${idx}"] on the page.`,
    );
    await el.click();
  },
);

When(
  "I click the footnote ref with index {string} inside a callout",
  async function (this: EmanoteWorld, idx: string) {
    const handle = await this.page.evaluateHandle((i) => {
      const refs = document.querySelectorAll(
        `[data-callout] sup[data-footnote-ref="${i}"]`,
      );
      return (refs[0] as HTMLElement) ?? null;
    }, idx);
    const el = handle.asElement();
    assert.ok(
      el,
      `No [data-callout] sup[data-footnote-ref="${idx}"] on the page.`,
    );
    await el.click();
  },
);

Then(
  "the footnote popup contains {string}",
  async function (this: EmanoteWorld, needle: string) {
    const popover = this.page.locator(POPOVER_SEL);
    await popover.waitFor({ state: "attached", timeout: 5_000 });
    const isOpen = await popover.evaluate((el) =>
      (el as HTMLElement).matches(":popover-open"),
    );
    assert.ok(
      isOpen,
      "Popover element exists but is not open — showPopover() likely failed or the click didn't reach the handler.",
    );
    const text = (await popover.textContent()) ?? "";
    assert.ok(
      text.includes(needle),
      `Popover did not contain ${JSON.stringify(needle)}. Got: ${JSON.stringify(text)}.`,
    );
  },
);

// The popup body's Tailwind classes (bg-white, rounded-lg, shadow-xl,
// etc.) are applied by JS at runtime — they never appear in any
// generated HTML attribute, so the static-mode Tailwind CLI doesn't
// see them and silently drops the corresponding rules. The visible
// symptom is a popup with no background fill, no shadow, no rounding —
// just a thin default border around plain text. Asserting on a
// concrete computed style (the body's background-color) catches that
// regression: the JS sets `bg-white dark:bg-gray-900`, which compiles
// to a non-transparent rgb()/oklab() value. If Tailwind didn't pick up
// the class, getComputedStyle returns rgba(0,0,0,0).
Then(
  "the footnote popup body has a non-transparent background",
  async function (this: EmanoteWorld) {
    const popover = this.page.locator(POPOVER_SEL);
    const bg = await popover.evaluate((el) => {
      const body = el.firstElementChild as HTMLElement | null;
      if (!body) return null;
      return getComputedStyle(body).backgroundColor;
    });
    assert.ok(bg, "Popover has no body element — script setup regressed.");
    assert.ok(
      bg !== "rgba(0, 0, 0, 0)" && bg !== "transparent",
      `Popover body background is ${JSON.stringify(bg)} — Tailwind dropped the popup classes (likely because the static-mode CSS compile didn't scan the JS module that applies them; see Tailwind.hs's @source for _emanote-static/js).`,
    );
  },
);

// Print-mode footnotes. The popup is screen-only; the hidden <aside
// data-footnote-list> is revealed by `print:block` on paper so printed
// copies still carry the cited bodies.
async function countVisibleFootnoteAsides(page: EmanoteWorld["page"]) {
  return page.evaluate(
    () =>
      Array.from(
        document.querySelectorAll("aside[data-footnote-list]"),
      ).filter((a) => getComputedStyle(a as Element).display !== "none").length,
  );
}

Then(
  "no footnote list is visible on screen",
  async function (this: EmanoteWorld) {
    const count = await countVisibleFootnoteAsides(this.page);
    assert.strictEqual(
      count,
      0,
      `Expected no visible <aside data-footnote-list> on screen; got ${count}. The popup is the only footnote UI on screen — a visible aside means hidden/print:block got reverted.`,
    );
  },
);

When(
  "the page is emulated as print media",
  async function (this: EmanoteWorld) {
    await this.page.emulateMedia({ media: "print" });
  },
);

Then(
  "at least one footnote list is visible",
  async function (this: EmanoteWorld) {
    const count = await countVisibleFootnoteAsides(this.page);
    assert.ok(
      count > 0,
      `Expected at least one <aside data-footnote-list> visible under print emulation; got ${count}. The print:block variant on the aside is missing or the parent hides it regardless.`,
    );
  },
);

Then(
  "the printed footnote list contains {string}",
  async function (this: EmanoteWorld, needle: string) {
    const text = await this.page.evaluate(
      () =>
        Array.from(document.querySelectorAll("aside[data-footnote-list]"))
          .filter((a) => getComputedStyle(a as Element).display !== "none")
          .map((a) => (a as HTMLElement).textContent ?? "")
          .join(" "),
    );
    assert.ok(
      text.includes(needle),
      `Visible print-mode footnote list did not contain ${JSON.stringify(
        needle,
      )}. Got: ${JSON.stringify(text.slice(0, 200))}.`,
    );
  },
);

// Theme toggle button lives in the sidebar header (sidebar.tpl) and the
// minimal layout header (layouts/default.tpl); both wire it via inline
// onclick="window.emanote.theme.toggle()" with title="Toggle dark mode".
// Click the first one Playwright finds — both fire the same handler.
When("I click the theme toggle", async function (this: EmanoteWorld) {
  await this.page.locator('button[title="Toggle dark mode"]').first().click();
});

Then(
  "the documentElement has class {string}",
  async function (this: EmanoteWorld, cls: string) {
    const has = await this.page.evaluate(
      (c) => document.documentElement.classList.contains(c),
      cls,
    );
    assert.ok(
      has,
      `Expected <html> to have class ${JSON.stringify(cls)}; classList was ${JSON.stringify(
        await this.page.evaluate(() => document.documentElement.className),
      )}. The theme-toggle module either failed to load or its onclick handler did not run.`,
    );
  },
);

Then(
  "localStorage {string} is {string}",
  async function (this: EmanoteWorld, key: string, expected: string) {
    const value = await this.page.evaluate(
      (k) => localStorage.getItem(k),
      key,
    );
    assert.strictEqual(
      value,
      expected,
      `Expected localStorage[${JSON.stringify(key)}] to be ${JSON.stringify(expected)}; got ${JSON.stringify(value)}. The persistence step in window.emanote.theme.toggle either threw silently or never ran.`,
    );
  },
);

// code-copy.js wires a button into every <pre> that has a child <code>.
// Asserting parity (#buttons === #pre>code) catches both regressions:
// missing buttons (selector mismatch, module not loaded) and duplicates
// (idempotency-guard regressed in morph.js's dataset sentinel).
Then(
  "every <pre> with a child <code> has a .code-copy-button",
  async function (this: EmanoteWorld) {
    // Module loads via <script type="module"> which is defer-by-default —
    // wait for at least one button before counting.
    await this.page
      .locator("pre > .code-copy-button")
      .first()
      .waitFor({ state: "attached", timeout: 5_000 });
    const counts = await this.page.evaluate(() => ({
      preWithCode: document.querySelectorAll("pre:has(> code)").length,
      buttons: document.querySelectorAll("pre > .code-copy-button").length,
    }));
    assert.ok(
      counts.preWithCode > 0,
      `Fixture has zero <pre><code> blocks; nothing to assert against.`,
    );
    assert.strictEqual(
      counts.buttons,
      counts.preWithCode,
      `Expected one .code-copy-button per <pre><code>, got ${counts.buttons} buttons for ${counts.preWithCode} blocks. Mismatch means either the module didn't run, the selector drifted, or the dataset-sentinel idempotency guard regressed and produced duplicates.`,
    );
  },
);

When(
  "I navigate via Ema to {string}",
  async function (this: EmanoteWorld, path: string) {
    await morphNav(this.page, path);
  },
);

// TOC scroll-spy: position a target heading inside the IntersectionObserver
// "active band" (rootMargin "-80px 0px -60% 0px" → y=80..288 on a 720px
// viewport), then assert its TOC link is highlighted. Putting the heading
// at y=200 sits it firmly inside the band, so the firstVisible branch in
// pickActive() picks it deterministically — no subpixel fragility on the
// `top < 0` boundary that "scroll to viewport top" semantics would have.
When(
  "I scroll the heading with id {string} into the active band",
  async function (this: EmanoteWorld, headingId: string) {
    await this.page.evaluate((id) => {
      const el = document.getElementById(id);
      if (!el) throw new Error(`No element with id ${JSON.stringify(id)}`);
      const headingTopInDoc = el.getBoundingClientRect().top + window.scrollY;
      window.scrollTo({ top: headingTopInDoc - 200, behavior: "instant" });
    }, headingId);
  },
);

Then(
  "the TOC link for {string} has class {string}",
  async function (this: EmanoteWorld, hrefSuffix: string, cls: string) {
    // The TOC link's href is absolute (resolves against <base>); match by
    // suffix to stay independent of the served origin.
    const link = this.page
      .locator(`#toc a.--ema-toc[href$="${hrefSuffix}"]`)
      .first();
    await link.waitFor({ state: "attached", timeout: 5_000 });
    // Auto-retrying expectation — the IntersectionObserver fires async
    // after scroll, so the active class arrives a frame or two later.
    try {
      await this.page.waitForFunction(
        ({ suffix, mark }) => {
          const a = document.querySelector(
            `#toc a.--ema-toc[href$="${suffix}"]`,
          );
          return a?.classList.contains(mark) ?? false;
        },
        { suffix: hrefSuffix, mark: cls },
        { timeout: 3_000 },
      );
    } catch {
      const allMarked = await this.page.evaluate(
        (mark) =>
          Array.from(document.querySelectorAll(`#toc a.--ema-toc.${mark}`))
            .map((a) => (a as HTMLAnchorElement).href)
            .join(", ") || "(none)",
        cls,
      );
      throw new Error(
        `Expected TOC link with href ending ${JSON.stringify(hrefSuffix)} to have class ${JSON.stringify(cls)}. Currently active links: ${allMarked}. Either the IntersectionObserver never fired (toc-spy.js not loaded / module-mode timing changed the observed-set computation) or pickActive picked a different section than expected.`,
      );
    }
  },
);

// Stork search modal. The container starts with `hidden` on the
// class list (set in components/stork/stork-search.tpl); toggleSearch
// in stork.js flips it. Asserting the class membership is more
// reliable than computed visibility because the container is
// position:fixed with a backdrop — getComputedStyle sees `display:
// block` either way; only the .hidden class hides it.
const STORK_CONTAINER_SEL = "#stork-search-container";

Then(
  "the Stork search modal is {string}",
  async function (this: EmanoteWorld, state: string) {
    const container = this.page.locator(STORK_CONTAINER_SEL);
    await container.waitFor({ state: "attached", timeout: 5_000 });
    // Module load is defer + dynamic-import async; give the click
    // handler a couple frames to land before reading state.
    try {
      await this.page.waitForFunction(
        ({ sel, want }) => {
          const el = document.querySelector(sel);
          if (!el) return false;
          const hidden = el.classList.contains("hidden");
          return want === "hidden" ? hidden : !hidden;
        },
        { sel: STORK_CONTAINER_SEL, want: state },
        { timeout: 3_000 },
      );
    } catch {
      const actual = await container.evaluate((el) =>
        el.classList.contains("hidden") ? "hidden" : "visible",
      );
      throw new Error(
        `Expected Stork modal to be ${JSON.stringify(state)}; was ${JSON.stringify(actual)}. Either stork.js didn't load (check importmap), the keyboard listener / data-emanote-stork-toggle delegation didn't fire, or toggleSearch didn't update the .hidden class on ${STORK_CONTAINER_SEL}.`,
      );
    }
  },
);

When("I press {string}", async function (this: EmanoteWorld, key: string) {
  await this.page.keyboard.press(key);
});

When(
  "I click the Stork search trigger",
  async function (this: EmanoteWorld) {
    // Several buttons carry data-emanote-stork-toggle (sidebar +
    // breadcrumbs); the breadcrumbs button is mobile-only (md:hidden)
    // and at the test viewport (1280×720, md+) only the sidebar
    // button is visible. Filter to the visible one — `.first()` alone
    // would pick the hidden breadcrumbs button by document order and
    // time out trying to click an invisible target.
    await this.page
      .locator("button[data-emanote-stork-toggle]:visible")
      .first()
      .click();
  },
);

// Stork dark/light theme mirror: stork.js's MutationObserver on
// <html>.class flips the wrapper between stork-wrapper-edible and
// stork-wrapper-edible-dark. Without one of those classes on the
// wrapper, the edible.css / edible-dark.css rules don't apply and
// the search dialog renders as unstyled inputs. Asserting on the
// presence of either class catches the morph-time regression where
// the new #stork-wrapper element gets no class because the observer
// only fires when <html>.class actually changes.
Then(
  "the Stork wrapper has the edible theme class",
  async function (this: EmanoteWorld) {
    const wrapper = this.page.locator("#stork-wrapper");
    await wrapper.waitFor({ state: "attached", timeout: 5_000 });
    const classes = (await wrapper.getAttribute("class")) ?? "";
    const ok =
      classes.includes("stork-wrapper-edible") ||
      classes.includes("stork-wrapper-edible-dark");
    assert.ok(
      ok,
      `Expected #stork-wrapper to carry stork-wrapper-edible{,-dark}; class list was ${JSON.stringify(classes)}. After Ema's morph nav the wrapper element is fresh and the JS theme-mirror only fires on <html>.class changes — if the user didn't flip themes, the new wrapper stays unstyled.`,
    );
  },
);
