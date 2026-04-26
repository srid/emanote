/**
 * `EMANOTE_MODE` — the single axis along which e2e runs differ.
 *
 *   - `live`   → spawn `emanote -L <fixture> run --port N`
 *   - `static` → `emanote -L <fixture> gen <tmp>` once, then serve `<tmp>`
 *   - `morph`  → live backend, but `When I open` route-switches via
 *                `window.ema.switchRoute` instead of `page.goto`
 *
 * Read once at module load and validated; downstream modules import the
 * narrowed `mode` constant.
 */

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
