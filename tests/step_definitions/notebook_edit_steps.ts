/**
 * Generic notebook-mutation and live-page-content steps shared by
 * every @hot-reload scenario regardless of dep kind.
 *
 * Lua-filter-specific steps (filter shape, late-bound note shape)
 * live in `lua_filter_steps.ts`. A future YAML-cascade or transclude
 * dep kind would add its own dep-shaped steps in a sibling file and
 * reuse the primitives here.
 */

import { Then, When } from "@cucumber/cucumber";
import * as fs from "node:fs";
import * as path from "node:path";
import * as assert from "node:assert";
import { EmanoteWorld } from "../support/world.ts";
import { stagedFixtureDir } from "../support/fixture.ts";

When("I delete {string}", function (fp: string) {
  fs.unlinkSync(path.join(stagedFixtureDir, fp));
});

Then(
  "the article body contains {string}",
  async function (this: EmanoteWorld, needle: string) {
    const text = (await this.page.textContent("body")) ?? "";
    assert.ok(
      text.includes(needle),
      `Expected page body to contain ${JSON.stringify(needle)}; first 400 chars: ${JSON.stringify(text.slice(0, 400))}.`,
    );
  },
);

// Tag-shaped assertion for nodes whose markup matters (e.g. <svg>
// produced by the bundled diagram filter). textContent stripping
// would hide the element, so we check the rendered DOM directly.
Then(
  "the article body contains an {string} element",
  async function (this: EmanoteWorld, tag: string) {
    const count = await this.page.locator(`article ${tag}`).count();
    assert.ok(
      count > 0,
      `Expected at least one <${tag}> inside <article>; got ${count}.`,
    );
  },
);

Then(
  "the article body does not contain {string}",
  async function (this: EmanoteWorld, needle: string) {
    const text = (await this.page.textContent("body")) ?? "";
    assert.ok(
      !text.includes(needle),
      `Page body unexpectedly contained ${JSON.stringify(needle)}; first 400 chars: ${JSON.stringify(text.slice(0, 400))}.`,
    );
  },
);

// Live-mode hot-reload assertion. After mutating a file in the
// staged notebook, the filesystem-watcher → patchModel → Ema-WS
// chain takes some time; we poll the live page DOM until the
// expected post-update marker appears. The default ceiling matches
// the cucumber step timeout (60s) but the green path lands within
// a couple seconds.
Then(
  "the article body contains {string} within {int} seconds",
  async function (this: EmanoteWorld, needle: string, seconds: number) {
    await this.page.waitForFunction(
      (n: string) => document.body.innerText.includes(n),
      needle,
      { timeout: seconds * 1000 },
    );
  },
);

// Used when a scenario creates a brand-new note: opening the route
// before emanote has parsed it lands on the "missing link" template,
// which doesn't morph into the real page when the route later
// appears (the missing-link variant doesn't carry the morph payload
// that converts a not-yet-found route to a found one). Poll the
// route via HTTP until it reports the expected content, then open
// in the browser.
When(
  "I wait for {string} to contain {string}",
  async function (this: EmanoteWorld, url: string, needle: string) {
    const deadline = Date.now() + 15_000;
    let lastStatus: number | undefined;
    while (Date.now() < deadline) {
      const resp = await this.page.request.get(url);
      lastStatus = resp.status();
      if (resp.ok()) {
        const body = await resp.text();
        if (body.includes(needle)) return;
      }
      await new Promise((r) => setTimeout(r, 200));
    }
    throw new Error(
      `Timed out waiting for ${url} to contain ${JSON.stringify(needle)} (last status=${String(lastStatus)}).`,
    );
  },
);
