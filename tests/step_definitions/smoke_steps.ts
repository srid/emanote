import { Given, When, Then } from "@cucumber/cucumber";
import { EmanoteWorld } from "../support/world.ts";
import * as assert from "node:assert";

When("I open {string}", async function (this: EmanoteWorld, url: string) {
  await this.page.goto(url, { waitUntil: "domcontentloaded" });
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
