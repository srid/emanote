import {
  World,
  setWorldConstructor,
  setDefaultTimeout,
} from "@cucumber/cucumber";
import type { Browser, BrowserContext, Page } from "playwright";

setDefaultTimeout(60_000);

export class EmanoteWorld extends World {
  browser!: Browser;
  context!: BrowserContext;
  page!: Page;
  notedPrimary500?: string;

  /** Computed value of `--color-primary-500` on `document.documentElement`.
   *  Non-empty iff the theme-remap alias and the target palette both made
   *  it into the final stylesheet — which is the #633-class regression
   *  signal. */
  async resolvedPrimary500(): Promise<string> {
    return this.page.evaluate(() =>
      getComputedStyle(document.documentElement)
        .getPropertyValue("--color-primary-500")
        .trim(),
    );
  }
}

setWorldConstructor(EmanoteWorld);
