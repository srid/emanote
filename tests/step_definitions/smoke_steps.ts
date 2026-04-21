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

// Guards issue #360 (duplicate footnote ids under note embedding). The
// outer + inner both render through pandocSplice, which numbers footnotes
// from 1 per document; before the fix, the embed inherited the outer's
// id namespace and produced `id="fn1"` twice on the same page.
Then(
  "every element id on the page is unique",
  async function (this: EmanoteWorld) {
    const duplicates = await this.page.evaluate(() => {
      const counts = new Map<string, number>();
      for (const el of document.querySelectorAll("[id]")) {
        const id = el.id;
        counts.set(id, (counts.get(id) ?? 0) + 1);
      }
      return [...counts.entries()].filter(([, n]) => n > 1);
    });
    assert.strictEqual(
      duplicates.length,
      0,
      `Duplicate element ids found (see #360): ${JSON.stringify(duplicates)}`,
    );
  },
);

Then(
  "at least one footnote id is present",
  async function (this: EmanoteWorld) {
    const count = await this.page
      .locator('[id^="fn"]')
      .count();
    assert.ok(
      count > 0,
      "Expected at least one footnote id on the page; the fixture's Markdown may have stopped producing footnotes.",
    );
  },
);
