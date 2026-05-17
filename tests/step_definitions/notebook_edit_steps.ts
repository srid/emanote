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
import { stagedFixtureDir, writeStaged } from "../support/fixture.ts";
import { pollUrl } from "../support/poll.ts";

When("I delete {string}", function (fp: string) {
  fs.unlinkSync(path.join(stagedFixtureDir, fp));
});

When(
  "I replace {string} with {string} in {string}",
  function (fromToken: string, toToken: string, fp: string) {
    const target = path.join(stagedFixtureDir, fp);
    const original = fs.readFileSync(target, "utf8");
    assert.ok(
      original.includes(fromToken),
      `${fp} does not contain ${JSON.stringify(fromToken)}.`,
    );
    fs.writeFileSync(target, original.replaceAll(fromToken, toToken));
  },
);

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

// .emanoteignore hot-reload (issue #739) wants to rewrite a whole
// configuration file rather than patch a substring — easier with a
// Gherkin doc-string than with the substring-based `I replace …` step.
When("I write the file {string} with:", function (fp: string, body: string) {
  writeStaged(fp, body);
});

// Like `the article body contains … within N seconds` but polls a URL
// over HTTP instead of an already-open page. .emanoteignore hot-reload
// scenarios need this because a newly re-included route may never have
// been opened in the browser; we just want to assert that fetching it
// reflects the updated ignore set.
Then(
  "the URL {string} contains {string} within {int} seconds",
  async function (
    this: EmanoteWorld,
    url: string,
    needle: string,
    seconds: number,
  ) {
    await pollUrl(
      this.page,
      url,
      seconds * 1000,
      ({ ok, body }) => ok && body !== null && body.includes(needle),
      ({ body }) =>
        new Error(
          `URL ${url} did not contain ${JSON.stringify(needle)} within ${seconds}s; last body prefix: ${JSON.stringify((body ?? "").slice(0, 200))}.`,
        ),
    );
  },
);

// Inverse of the above: poll until the URL no longer includes the
// marker (body changed, or the route became unreachable). Used by the
// "pattern added → note hidden" branch of the .emanoteignore hot-reload
// matrix.
Then(
  "the URL {string} stops containing {string} within {int} seconds",
  async function (
    this: EmanoteWorld,
    url: string,
    needle: string,
    seconds: number,
  ) {
    await pollUrl(
      this.page,
      url,
      seconds * 1000,
      ({ ok, body }) => !ok || (body !== null && !body.includes(needle)),
      () =>
        new Error(
          `URL ${url} continued to contain ${JSON.stringify(needle)} after ${seconds}s.`,
        ),
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
    await pollUrl(
      this.page,
      url,
      15_000,
      ({ ok, body }) => ok && body !== null && body.includes(needle),
      ({ status }) =>
        new Error(
          `Timed out waiting for ${url} to contain ${JSON.stringify(needle)} (last status=${status}).`,
        ),
    );
  },
);
