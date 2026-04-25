import { When, Then } from "@cucumber/cucumber";
import { EmanoteWorld } from "../support/world.ts";
import * as assert from "node:assert";

// Callouts are uniquely identified on the fixture by `data-callout="<type>"`.
// Plain callouts render as <div>; foldable as <details>. The fold state is
// expressed by (a) the element being <details> at all and (b) the presence
// of the `open` boolean attribute.

async function calloutHandle(page: EmanoteWorld["page"], type: string) {
  const handle = await page.evaluateHandle((t) => {
    return document.querySelector(`[data-callout="${t}"]`);
  }, type);
  const el = handle.asElement();
  assert.ok(el, `No element with data-callout="${type}" on the page.`);
  return el;
}

Then(
  'the callout with type {string} is non-foldable',
  async function (this: EmanoteWorld, type: string) {
    const el = await calloutHandle(this.page, type);
    const tag = await el.evaluate((node) => (node as Element).tagName);
    assert.strictEqual(
      tag,
      "DIV",
      `Expected the [data-callout="${type}"] callout to render as a <div> (non-foldable); got <${tag.toLowerCase()}>. A <details> here means a stray '+' or '-' leaked into the type parser.`,
    );
  },
);

Then(
  'the callout with type {string} is foldable and initially expanded',
  async function (this: EmanoteWorld, type: string) {
    const el = await calloutHandle(this.page, type);
    const { tag, open } = await el.evaluate((node) => ({
      tag: (node as Element).tagName,
      open: (node as HTMLDetailsElement).open,
    }));
    assert.strictEqual(
      tag,
      "DETAILS",
      `Expected [data-callout="${type}"] to render as <details> for the foldable form; got <${tag.toLowerCase()}>.`,
    );
    assert.strictEqual(
      open,
      true,
      `Expected [data-callout="${type}"] to be initially expanded (open=true); got open=false. The '+' fold suffix should map to the 'expanded' branch.`,
    );
  },
);

Then(
  'the callout with type {string} is foldable and initially collapsed',
  async function (this: EmanoteWorld, type: string) {
    const el = await calloutHandle(this.page, type);
    const { tag, open } = await el.evaluate((node) => ({
      tag: (node as Element).tagName,
      open: (node as HTMLDetailsElement).open,
    }));
    assert.strictEqual(
      tag,
      "DETAILS",
      `Expected [data-callout="${type}"] to render as <details> for the foldable form; got <${tag.toLowerCase()}>.`,
    );
    assert.strictEqual(
      open,
      false,
      `Expected [data-callout="${type}"] to be initially collapsed (open=false); got open=true. The '-' fold suffix should map to the 'collapsed' branch.`,
    );
  },
);

// Body visibility: a closed <details> hides everything except <summary>, so
// the .callout-content div has zero rendered height. Use bounding box rather
// than getComputedStyle because <details> doesn't change `display` on its
// children — the browser hides them via internal layout.
async function bodyHeight(page: EmanoteWorld["page"], type: string) {
  return page.evaluate((t) => {
    const el = document.querySelector(`[data-callout="${t}"]`);
    if (!el) return null;
    const body = el.querySelector(".callout-content");
    if (!body) return null;
    return (body as HTMLElement).getBoundingClientRect().height;
  }, type);
}

Then(
  'the body of the callout with type {string} is visible',
  async function (this: EmanoteWorld, type: string) {
    const h = await bodyHeight(this.page, type);
    assert.ok(
      h !== null && h > 0,
      `Expected the .callout-content of [data-callout="${type}"] to render with non-zero height; got ${JSON.stringify(h)}. A 0 height inside an open <details> means the body block isn't being expanded.`,
    );
  },
);

Then(
  'the body of the callout with type {string} is hidden',
  async function (this: EmanoteWorld, type: string) {
    const h = await bodyHeight(this.page, type);
    assert.strictEqual(
      h,
      0,
      `Expected the .callout-content of [data-callout="${type}"] to be collapsed (height 0); got ${JSON.stringify(h)}. A non-zero height inside a closed <details> means the 'open' attribute is leaking in or CSS overrode the native folding.`,
    );
  },
);

When(
  'I click the summary of the callout with type {string}',
  async function (this: EmanoteWorld, type: string) {
    const summary = this.page.locator(`[data-callout="${type}"] > summary`);
    await summary.click();
  },
);

Then(
  'the callout with type {string} contains a nested callout with type {string}',
  async function (this: EmanoteWorld, outer: string, inner: string) {
    const present = await this.page.evaluate(
      ({ outer, inner }) => {
        const o = document.querySelector(`[data-callout="${outer}"]`);
        if (!o) return "missing-outer";
        const candidates = o.querySelectorAll(`[data-callout="${inner}"]`);
        return candidates.length > 0 ? "ok" : "missing-inner";
      },
      { outer, inner },
    );
    assert.strictEqual(
      present,
      "ok",
      present === "missing-outer"
        ? `No outer callout with data-callout="${outer}" on the page.`
        : `Outer callout [data-callout="${outer}"] exists but contains no nested [data-callout="${inner}"]. Either the inner blockquote isn't being recognized as a callout (renderer regression), or the parser is consuming the inner BlockQuote into the outer's title/body header.`,
    );
  },
);
