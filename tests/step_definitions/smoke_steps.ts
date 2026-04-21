import { Given, When, Then } from "@cucumber/cucumber";
import { EmanoteWorld } from "../support/world.ts";
import * as assert from "node:assert";

/** Max time we'll wait for the Tailwind CDN (live mode) or the statically
 *  generated stylesheet (static mode) to make `--color-primary-500`
 *  resolve on `:root`. Correct setups land within a few hundred ms; the
 *  #633-class regression never resolves at all, so a generous ceiling
 *  here costs nothing on the green path and fails loudly on the red path. */
const CSS_READY_TIMEOUT = 10_000;

async function waitForPrimaryResolved(
  world: EmanoteWorld,
): Promise<string> {
  await world.page.waitForFunction(
    () =>
      getComputedStyle(document.documentElement)
        .getPropertyValue("--color-primary-500")
        .trim().length > 0,
    null,
    { timeout: CSS_READY_TIMEOUT },
  );
  return world.resolvedPrimary500();
}

When("I open {string}", async function (this: EmanoteWorld, url: string) {
  await this.page.goto(url, { waitUntil: "domcontentloaded" });
});

Given(
  "I note the resolved primary palette at {string}",
  async function (this: EmanoteWorld, url: string) {
    await this.page.goto(url, { waitUntil: "domcontentloaded" });
    this.notedPrimary500 = await waitForPrimaryResolved(this);
  },
);

Then(
  "the primary palette custom property resolves to a non-empty value",
  async function (this: EmanoteWorld) {
    try {
      const value = await waitForPrimaryResolved(this);
      assert.ok(value);
    } catch (e) {
      throw new Error(
        `--color-primary-500 never resolved on :root within ${CSS_READY_TIMEOUT}ms. The theme-remap alias or the target palette var is missing (see #633). Underlying: ${String(e)}`,
      );
    }
  },
);

Then(
  "the resolved primary palette differs from the noted value",
  async function (this: EmanoteWorld) {
    assert.ok(
      this.notedPrimary500,
      "Noted primary palette is empty — the Given step did not record a baseline.",
    );
    const value = await waitForPrimaryResolved(this);
    assert.notStrictEqual(
      value,
      this.notedPrimary500,
      `Expected per-page template.theme override to change --color-primary-500; both pages resolved to ${JSON.stringify(value)}.`,
    );
  },
);
