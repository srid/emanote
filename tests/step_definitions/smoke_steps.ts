import { Given, When, Then } from "@cucumber/cucumber";
import { EmanoteWorld } from "../support/world.ts";
import * as assert from "node:assert";

When("I open {string}", async function (this: EmanoteWorld, url: string) {
  await this.page.goto(url, { waitUntil: "domcontentloaded" });
});

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
