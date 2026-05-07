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

/** Modes that run a live `emanote run` backend — i.e. provide a
 *  WebSocket and `window.ema`, plus live-only behavior such as the
 *  ambiguous-link candidate list. `static` is the only mode that lacks
 *  them. New backend-capable modes get appended here. */
export const NON_STATIC_MODES: readonly Mode[] = ["live", "morph"];

/** Register a Cucumber Before hook that skips scenarios tagged `tag`
 *  whenever the current run mode is not in `requiredModes`. Generalises
 *  to N-way gating — e.g. a future `@morph-only` would pass `["morph"]`. */
export function skipUnlessMode(
  tag: string,
  requiredModes: readonly Mode[],
): void {
  Before({ tags: tag }, function () {
    if (!requiredModes.includes(mode)) return "skipped" as const;
  });
}
