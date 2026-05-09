/**
 * Mode-aware navigation primitives. Step definitions go through
 * `openRoute`; the morph branch route-switches via the Ema WS shim
 * instead of a fresh page load.
 */

import type { Page } from "playwright";
import { mode } from "./mode.ts";

// Wait for Ema's WS shim — `ema.ready` (srid/ema#181) resolves after
// the WS handshake. Awaiting it avoids racing the first `switchRoute`.
async function waitForEmaReady(page: Page): Promise<void> {
  await page.waitForFunction(() => !!(window as any).ema?.switchRoute);
  await page.evaluate(() => (window as any).ema.ready);
}

export async function primeMorph(page: Page): Promise<void> {
  await page.goto("/", { waitUntil: "domcontentloaded" });
  await waitForEmaReady(page);
}

// All ready+switchRoute+EMAHotReload work in a single browser-side
// evaluate. Splitting the await-ready and the switchRoute into separate
// CDP round-trips opens a window where the page can navigate (morph
// involves a script reload) and Playwright's evaluate sees the
// execution context destroyed before EMAHotReload fires.
export async function morphNav(page: Page, url: string): Promise<void> {
  await page.evaluate(async (p) => {
    while (!(window as any).ema?.switchRoute) {
      await new Promise((r) => setTimeout(r, 25));
    }
    await (window as any).ema.ready;
    await new Promise<void>((resolve) => {
      window.addEventListener("EMAHotReload", () => resolve(), {
        once: true,
      });
      (window as any).ema.switchRoute(p);
    });
  }, url);
}

export async function openRoute(page: Page, url: string): Promise<void> {
  if (mode === "morph") {
    await morphNav(page, url);
  } else {
    await page.goto(url, { waitUntil: "domcontentloaded" });
  }
}
