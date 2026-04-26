/**
 * Mode-aware navigation primitives. Step definitions stay mode-agnostic
 * by going through `openRoute`; in `morph` mode the page is route-
 * switched via `window.ema.switchRoute`, in `live` and `static` modes
 * it's a fresh `page.goto`. The priming step (`primeMorph`) is what a
 * Before hook runs once per scenario in morph mode so the first
 * `openRoute` has a `window.ema` to switch through.
 */

import type { Page } from "playwright";
import { mode } from "./mode.ts";

/** Wait for Ema's WS shim to be ready: until `window.ema.switchRoute`
 *  exists, then await `window.ema.ready` (PR srid/ema#181) so the next
 *  caller doesn't race the WS handshake. The shim's `init()` runs on
 *  module load and is usually a no-op wait, but the gate is cheap
 *  insurance against page-load timing. */
async function waitForEmaReady(page: Page): Promise<void> {
  await page.waitForFunction(() => !!(window as any).ema?.switchRoute);
  await page.evaluate(() => (window as any).ema.ready);
}

/** Prime morph mode for a fresh scenario: land on `/` via a real page
 *  load and wait for the WS shim. Subsequent `openRoute` calls then
 *  morph-switch instead of reloading. */
export async function primeMorph(page: Page): Promise<void> {
  await page.goto("/", { waitUntil: "domcontentloaded" });
  await waitForEmaReady(page);
}

/** Drive an Ema-internal morph navigation: wait for the WS shim, then
 *  switch the route via `window.ema.switchRoute` and wait for
 *  `EMAHotReload` (fired by the shim after morph + script reload
 *  completes) so the next step sees the new DOM, not the in-flight one. */
export async function morphNav(page: Page, url: string): Promise<void> {
  await waitForEmaReady(page);
  await page.evaluate(
    (p) =>
      new Promise<void>((resolve) => {
        window.addEventListener("EMAHotReload", () => resolve(), {
          once: true,
        });
        (window as any).ema.switchRoute(p);
      }),
    url,
  );
}

/** Open a route — the verb step definitions call. In `live` and
 *  `static` modes this is a fresh `page.goto`; in `morph` mode it's an
 *  Ema-internal route switch. Step definitions stay mode-agnostic by
 *  going through this helper. */
export async function openRoute(page: Page, url: string): Promise<void> {
  if (mode === "morph") {
    await morphNav(page, url);
  } else {
    await page.goto(url, { waitUntil: "domcontentloaded" });
  }
}
