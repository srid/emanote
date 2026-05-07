/**
 * `EMANOTE_MODE` — the single axis along which e2e runs differ.
 *
 *   - `live`   → spawn `emanote -L <fixture> run --port N`
 *   - `static` → `emanote -L <fixture> gen <tmp>` once, then serve `<tmp>`
 *   - `morph`  → live backend, but `When I open` route-switches via
 *                `window.ema.switchRoute` instead of `page.goto`
 */

import { Before } from "@cucumber/cucumber";

export type Mode = "live" | "static" | "morph";

export function requireEnv(name: string): string {
  const v = process.env[name];
  if (!v) throw new Error(`${name} must be set`);
  return v;
}

const rawMode = requireEnv("EMANOTE_MODE");
if (rawMode !== "live" && rawMode !== "static" && rawMode !== "morph") {
  throw new Error(
    `EMANOTE_MODE must be "live", "static", or "morph" (got ${JSON.stringify(rawMode)})`,
  );
}

export const mode: Mode = rawMode;

/** Register a Cucumber Before hook that skips scenarios tagged `tag`
 *  whenever the current run mode is not in `requiredModes`. The Set form
 *  generalises to N-way gating — e.g. a future `@morph-only` would just
 *  pass `new Set(["morph"])`. */
export function skipUnlessMode(tag: string, requiredModes: Set<Mode>): void {
  Before({ tags: tag }, function () {
    if (!requiredModes.has(mode)) return "skipped" as const;
  });
}
