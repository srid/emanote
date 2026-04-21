import {
  World,
  setWorldConstructor,
  setDefaultTimeout,
} from "@cucumber/cucumber";
import type { Browser, BrowserContext, Page } from "playwright";

setDefaultTimeout(60_000);

/** Max time we'll wait for the Tailwind CDN (live mode) or the statically
 *  generated stylesheet (static mode) to make `--color-primary-500`
 *  resolve on `:root`. Correct setups land within a few hundred ms; the
 *  #633-class regression never resolves at all, so a generous ceiling
 *  here costs nothing on the green path and fails loudly on the red path. */
const CSS_READY_TIMEOUT = 10_000;

export class EmanoteWorld extends World {
  browser!: Browser;
  context!: BrowserContext;
  page!: Page;
  notedPrimary500?: string;

  /** Poll `--color-primary-500` on `:root` until it resolves to a
   *  non-empty value, then return it. Times out with the regression
   *  signal for #633 (theme-remap alias or target palette var missing). */
  async waitForPrimaryResolved(): Promise<string> {
    const handle = await this.page.waitForFunction(
      (prop: string) => {
        const v = getComputedStyle(document.documentElement)
          .getPropertyValue(prop)
          .trim();
        return v.length > 0 ? v : null;
      },
      "--color-primary-500",
      { timeout: CSS_READY_TIMEOUT },
    );
    return handle.jsonValue() as Promise<string>;
  }
}

setWorldConstructor(EmanoteWorld);
