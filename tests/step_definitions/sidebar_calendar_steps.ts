/**
 * Step definitions for the sidebar month-calendar widget (issue #700).
 *
 * The widget swaps a sidebar tree subtree (`<div class="emanote-tree-children">`)
 * whose children are all daily-note leaves of one month for a 7-column
 * calendar grid. Marker class on the rendered wrapper is
 * `.emanote-sidebar-calendar`. Filled cells are anchors carrying
 * `aria-label="YYYY-MM-DD — title"`; missing days are bare `<span>`s.
 *
 * The widget runs purely in JS post-render, so a live mode + JS-enabled
 * page is required. In static mode the JS bundle still ships, so the
 * scenarios run there too — only `@morph` cases are skipped statically
 * (no WebSocket, no `window.ema`).
 */

import { Given, Then } from "@cucumber/cucumber";
import { EmanoteWorld } from "../support/world.ts";
import * as assert from "node:assert";

const CALENDAR_SEL = ".emanote-sidebar-calendar";
const SIDEBAR_TREE_ROOT_SEL = "#sidebar-tree, nav";

// Wait for at least one calendar wrapper to appear. The widget runs on
// `ready`, so the wrapper materialises a frame or two after DOMContentLoaded.
async function waitForCalendar(page: EmanoteWorld["page"]): Promise<void> {
  await page
    .locator(CALENDAR_SEL)
    .first()
    .waitFor({ state: "attached", timeout: 5_000 });
}

async function getCalendarDay(
  page: EmanoteWorld["page"],
  day: number,
): Promise<ReturnType<EmanoteWorld["page"]["locator"]>> {
  await waitForCalendar(page);
  const cell = page.locator(`${CALENDAR_SEL} [data-day="${day}"]`).first();
  await cell.waitFor({ state: "attached", timeout: 5_000 });
  return cell;
}

Given(
  "the browser date is {string}",
  async function (this: EmanoteWorld, isoDate: string) {
    assert.match(
      isoDate,
      /^\d{4}-\d{2}-\d{2}$/,
      "Browser date fixture must be YYYY-MM-DD.",
    );
    await this.page.addInitScript({
      content: `
        (() => {
          const date = ${JSON.stringify(isoDate)};
          const RealDate = Date;
          const [year, month, day] = date.split("-").map(Number);
          const fixedTime = new RealDate(year, month - 1, day, 12).getTime();
          function MockDate(...args) {
            if (new.target) {
              return args.length === 0
                ? new RealDate(fixedTime)
                : new RealDate(...args);
            }
            return new RealDate(fixedTime).toString();
          }
          Object.setPrototypeOf(MockDate, RealDate);
          MockDate.prototype = RealDate.prototype;
          MockDate.now = () => fixedTime;
          window.Date = MockDate;
        })();
      `,
    });
  },
);

Then(
  "the sidebar month calendar is visible with header {string}",
  async function (this: EmanoteWorld, header: string) {
    await waitForCalendar(this.page);
    const headerText = await this.page
      .locator(`${CALENDAR_SEL} > div`)
      .first()
      .textContent();
    assert.strictEqual(
      (headerText ?? "").trim(),
      header,
      `Expected sidebar calendar header ${JSON.stringify(header)}, got ${JSON.stringify(headerText)}. Either sidebar-calendar.js didn't run (importmap regression / module dropped from emanoteJsModuleNames) or buildCalendar's header text format changed.`,
    );
  },
);

Then(
  "the sidebar month calendar marks day {int} as the active route",
  async function (this: EmanoteWorld, day: number) {
    const cell = await getCalendarDay(this.page, day);
    const active = await cell.getAttribute("data-active-route");
    const ariaCurrent = await cell.getAttribute("aria-current");
    const cls = (await cell.getAttribute("class")) ?? "";
    assert.notStrictEqual(
      active,
      null,
      `Expected day ${day} to carry data-active-route. Current route highlighting likely failed to compare the cell href with window.location.pathname.`,
    );
    assert.strictEqual(
      ariaCurrent,
      "page",
      `Expected day ${day} to expose aria-current="page" for the active route.`,
    );
    assert.ok(
      cls.includes("bg-primary-700"),
      `Expected active route day ${day} to use the selected primary background; class=${JSON.stringify(cls)}.`,
    );
  },
);

Then(
  "the sidebar month calendar marks day {int} as today",
  async function (this: EmanoteWorld, day: number) {
    const cell = await getCalendarDay(this.page, day);
    const today = await cell.getAttribute("data-today");
    const iso = await cell.getAttribute("data-iso-date");
    const cls = (await cell.getAttribute("class")) ?? "";
    const browserDate = await this.page.evaluate(() => {
      const now = new Date();
      return `${now.getFullYear()}-${String(now.getMonth() + 1).padStart(2, "0")}-${String(now.getDate()).padStart(2, "0")}`;
    });
    assert.notStrictEqual(
      today,
      null,
      `Expected day ${day} to carry data-today. cell date=${JSON.stringify(iso)}, browser date=${JSON.stringify(browserDate)}. Today highlighting likely failed to compare the cell date with the browser's local date.`,
    );
    assert.ok(
      cls.includes("underline"),
      `Expected today day ${day} to include the visual today marker; class=${JSON.stringify(cls)}.`,
    );
  },
);

Then(
  "the sidebar month calendar links day {int} to {string}",
  async function (this: EmanoteWorld, day: number, hrefSubstring: string) {
    await waitForCalendar(this.page);
    // Filled cells carry aria-label "YYYY-MM-DD — title" — use the
    // numeric day as a substring anchor since title text varies per
    // fixture but the date prefix is deterministic.
    const dayPadded = String(day).padStart(2, "0");
    const cell = this.page
      .locator(`${CALENDAR_SEL} a[aria-label*="-${dayPadded} —"]`)
      .first();
    await cell.waitFor({ state: "attached", timeout: 5_000 });
    const href = await cell.getAttribute("href");
    assert.ok(
      href && href.includes(hrefSubstring),
      `Expected sidebar calendar day ${day} to link to ${JSON.stringify(hrefSubstring)}, got href=${JSON.stringify(href)}. Either classifyMonthGroup misread data-iso-date or createFilledCell dropped the URL.`,
    );
  },
);

Then(
  "the sidebar month calendar has {int} filled day cells",
  async function (this: EmanoteWorld, expected: number) {
    await waitForCalendar(this.page);
    // Filled cells are anchors carrying aria-label "YYYY-MM-DD — title".
    // Counting anchors inside the calendar wrapper is the cleanest
    // fixture-controlled assertion (3 daily notes ⇒ 3 filled cells).
    // Empty days and leading blanks are <span>s — both ignored here.
    const filled = await this.page.locator(`${CALENDAR_SEL} a[aria-label]`).count();
    assert.strictEqual(
      filled,
      expected,
      `Expected ${expected} filled day cells, got ${filled}. Either classifyMonthGroup dropped a leaf (data-iso-date attribute regressed in the splice) or createFilledCell stopped emitting anchors.`,
    );
  },
);

Then(
  "the sidebar has no plain link with text {string}",
  async function (this: EmanoteWorld, linkText: string) {
    // Once the calendar swaps in, the original `<a>2026-04-XX</a>`
    // tree leaves are gone — replaced by aria-label-only cell anchors.
    // `:has-text("2026-04-01")` would still match cell aria-labels
    // because Playwright's `:has-text` checks accessible name; instead
    // use exact textContent to find link bodies that hold the date as
    // visible text.
    await waitForCalendar(this.page);
    const count = await this.page.evaluate(
      ({ root, text }) => {
        const sidebar = document.querySelector(root);
        if (!sidebar) return -1;
        return Array.from(sidebar.querySelectorAll("a")).filter(
          (a) => (a.textContent ?? "").trim() === text,
        ).length;
      },
      { root: SIDEBAR_TREE_ROOT_SEL, text: linkText },
    );
    assert.notStrictEqual(
      count,
      -1,
      `Sidebar root (${SIDEBAR_TREE_ROOT_SEL}) not found on the page.`,
    );
    assert.strictEqual(
      count,
      0,
      `Expected no plain sidebar link with visible text ${JSON.stringify(linkText)}; found ${count}. The calendar should replace the linear list, not augment it — render() likely failed to clear wrapper.textContent before appending.`,
    );
  },
);

Then(
  "the sidebar tree has no calendar wrapper for non-month folders",
  async function (this: EmanoteWorld) {
    // From `/`, the sidebar root's `<div class="emanote-tree-children">`
    // holds the top-level entries (`calendar-test`, `dailyhost`, etc.) —
    // none all-daily, so no calendar should mount on it. classifyMonthGroup's
    // null-return path is exercised here.
    const count = await this.page.locator(CALENDAR_SEL).count();
    // `dailyhost/` itself contains 2 same-month daily notes, so when the
    // sidebar expands it (only happens on the active path), it would also
    // mount a calendar. From `/` the dailyhost subtree is collapsed by
    // default, so the children HTML isn't even emitted, and no calendar
    // mounts. Asserting zero here verifies that.
    assert.strictEqual(
      count,
      0,
      `Expected zero ${CALENDAR_SEL} elements on '/'; found ${count}. The widget mounted on a non-month-only subtree — classifyMonthGroup should reject any subtree whose children include non-daily notes (a folder, an article without data-iso-date, etc.).`,
    );
  },
);
