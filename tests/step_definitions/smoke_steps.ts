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

// texmath emits `<math xmlns="http://www.w3.org/1998/Math/MathML" display="...">`
// for every expression. Assert on the namespace *and* display attribute so a
// regression to raw `\(…\)` delimiters (for client-side MathJax/KaTeX) — which
// would produce 0 math elements — fails loudly.
const MATHML_NS = "http://www.w3.org/1998/Math/MathML";

Then(
  "the page contains an inline <math> element in the MathML namespace",
  async function (this: EmanoteWorld) {
    const count = await this.page
      .locator(`math[xmlns="${MATHML_NS}"][display="inline"]`)
      .count();
    assert.ok(
      count > 0,
      `Expected at least one inline MathML element; found ${count}. emanote.staticMath default may have regressed, or texmath's InlineMath → display="inline" mapping changed.`,
    );
  },
);

Then(
  "the page contains a block <math> element in the MathML namespace",
  async function (this: EmanoteWorld) {
    const count = await this.page
      .locator(`math[xmlns="${MATHML_NS}"][display="block"]`)
      .count();
    assert.ok(
      count > 0,
      `Expected at least one block MathML element; found ${count}. The $$…$$ display-math path is broken.`,
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
