/**
 * Mode-aware navigation primitives. Step definitions go through
 * `openRoute`; the morph branch route-switches via the Ema WS shim
 * instead of a fresh page load.
 */

import type { Page } from "playwright";
import { mode } from "./mode.ts";

// `ema.ready` (srid/ema#181) resolves after the WS handshake — awaiting
// it avoids racing the first `switchRoute` against handshake completion.
async function waitForEmaReady(page: Page): Promise<void> {
  await page.waitForFunction(() => !!(window as any).ema?.switchRoute);
  await page.evaluate(() => (window as any).ema.ready);
}

export async function primeMorph(page: Page): Promise<void> {
  await page.goto("/", { waitUntil: "domcontentloaded" });
  await waitForEmaReady(page);
}

export async function morphNav(page: Page, url: string): Promise<void> {
  await waitForEmaReady(page);
  await page.evaluate(
    (p) =>
      new Promise<void>((resolve) => {
        // EMAHotReload fires once the morph + script reload finishes.
        // Without this gate the next step sees the in-flight DOM.
        window.addEventListener("EMAHotReload", () => resolve(), {
          once: true,
        });
        (window as any).ema.switchRoute(p);
      }),
    url,
  );
}

export async function openRoute(page: Page, url: string): Promise<void> {
  if (mode === "morph") {
    await morphNav(page, url);
  } else {
    await page.goto(url, { waitUntil: "domcontentloaded" });
  }
}
