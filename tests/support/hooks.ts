/**
 * Cucumber lifecycle plumbing: backend start/stop, browser context,
 * scenario filtering, screenshot-on-failure.
 */

import {
  AfterAll,
  Before,
  BeforeAll,
  After,
  Status,
} from "@cucumber/cucumber";
import { chromium } from "playwright";
import type { Browser } from "playwright";
import getPort from "get-port";
// @ts-expect-error — serve-handler ships no TS types; its runtime shape
// is a single function and we call it with an untyped options object.
import serveHandler from "serve-handler";
import * as fs from "node:fs";
import * as http from "node:http";
import * as os from "node:os";
import * as path from "node:path";
import { spawn, type ChildProcess } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";
import { EmanoteWorld } from "./world.ts";
import { mode, requireEnv } from "./mode.ts";
import { primeMorph } from "./navigation.ts";

const emanoteBin = requireEnv("EMANOTE_BIN");

const fixtureDir = path.resolve(
  import.meta.dirname,
  "..",
  "fixtures",
  "notebook",
);

const runRoot = fs.mkdtempSync(path.join(os.tmpdir(), "emanote-e2e-"));

/** Active backend resource. Exactly one variant is live per run,
 *  determined by `EMANOTE_MODE`. Keeping both cases in one union
 *  means the "what's running" invariant is expressed by the type
 *  rather than by a pair of optional module-level fields. */
type BackendResource =
  | { kind: "live"; proc: ChildProcess }
  | { kind: "static"; server: http.Server };

let browser: Browser;
let baseUrl: string;
let backend: BackendResource | undefined;

/** Poll a URL until it returns a 200 response containing `needle`. Emanote
 *  cold-start is slow (Haskell + Tailwind compile on live-mode first
 *  render) and `needle` guards against racing the page before HTML has
 *  been produced — an open port with a 404 or partial response is not
 *  "ready". */
async function waitForHtml(
  url: string,
  needle: string,
  timeoutMs: number,
): Promise<void> {
  const deadline = Date.now() + timeoutMs;
  let lastErr: unknown;
  while (Date.now() < deadline) {
    try {
      const resp = await fetch(url);
      if (resp.ok) {
        const body = await resp.text();
        if (body.includes(needle)) return;
      }
    } catch (e) {
      lastErr = e;
    }
    await sleep(500);
  }
  throw new Error(
    `Backend did not serve ${url} containing ${JSON.stringify(needle)} within ${timeoutMs}ms (last error: ${String(lastErr)})`,
  );
}

async function startLive(): Promise<{ url: string; resource: BackendResource }> {
  const port = await getPort();
  // WS is enabled (no `--no-ws`) so scenarios tagged `@morph` can
  // exercise Ema's in-app morph navigation via `window.ema.switchRoute`.
  // The fixtures are static during a run, so the WS doesn't trigger
  // spurious morphs in non-morph scenarios.
  const proc = spawn(
    emanoteBin,
    ["-L", fixtureDir, "run", "--port", String(port)],
    { stdio: ["ignore", "pipe", "pipe"] },
  );
  proc.stderr?.on("data", (d: Buffer) =>
    process.stderr.write(`[emanote:live] ${d}`),
  );
  proc.on("exit", (code, sig) => {
    if (code !== 0 && code !== null) {
      process.stderr.write(
        `[emanote:live] exited unexpectedly (code=${code} sig=${sig})\n`,
      );
    }
  });
  const url = `http://127.0.0.1:${port}`;
  await waitForHtml(url, "emanote-theme-remap", 60_000);
  return { url, resource: { kind: "live", proc } };
}

async function startStatic(): Promise<{
  url: string;
  resource: BackendResource;
}> {
  const outDir = path.join(runRoot, "site");
  fs.mkdirSync(outDir, { recursive: true });
  await new Promise<void>((resolve, reject) => {
    const p = spawn(emanoteBin, ["-L", fixtureDir, "gen", outDir], {
      stdio: "inherit",
    });
    p.on("exit", (code) =>
      code === 0
        ? resolve()
        : reject(new Error(`emanote gen exited with code ${code}`)),
    );
  });
  const port = await getPort();
  const server = http.createServer((req, res) =>
    serveHandler(req, res, {
      public: outDir,
      cleanUrls: false,
      // serve-handler doesn't auto-serve index.html on `/` when cleanUrls
      // is off — it falls through to a directory listing instead. Without
      // this rewrite, `page.goto("/")` lands on a listing page with no
      // stylesheets, so the primary-palette probe times out with a
      // misleading #633-class error. Explicit rewrite keeps the fixture
      // behavior consistent with real deployments.
      rewrites: [{ source: "/", destination: "/index.html" }],
    }),
  );
  await new Promise<void>((resolve) => server.listen(port, resolve));
  return {
    url: `http://127.0.0.1:${port}`,
    resource: { kind: "static", server },
  };
}

/** BeforeAll owns the slowest work in the run: the emanote cold start
 *  in live mode is bounded by `waitForHtml`'s internal 60s, after which
 *  we still need to launch Chromium. The default cucumber timeout
 *  (`setDefaultTimeout` above) is the step-level budget, not adequate
 *  here — so give this hook a deliberate 180s ceiling instead of racing
 *  the ~60s default. */
BeforeAll({ timeout: 180_000 }, async () => {
  const started = mode === "static" ? await startStatic() : await startLive();
  baseUrl = started.url;
  backend = started.resource;
  browser = await chromium.launch({
    headless: process.env.HEADLESS !== "false",
    args: ["--no-sandbox", "--disable-setuid-sandbox", "--disable-gpu"],
  });
});

AfterAll(async () => {
  if (browser) await browser.close();
  if (backend?.kind === "live") {
    backend.proc.kill("SIGTERM");
  } else if (backend?.kind === "static") {
    const { server } = backend;
    await new Promise<void>((r) => server.close(() => r()));
  }
  try {
    fs.rmSync(runRoot, { recursive: true, force: true });
  } catch {
    // Best-effort cleanup.
  }
});

Before(async function (this: EmanoteWorld) {
  this.browser = browser;
  this.context = await browser.newContext({
    baseURL: baseUrl,
    viewport: { width: 1280, height: 720 },
  });
  this.page = await this.context.newPage();
});

const MORPH_TAG = "@morph";

// Static mode has no WebSocket and `window.ema` is undefined, so any
// `@morph` scenario would fail at the first `switchRoute` call.
Before({ tags: MORPH_TAG }, function () {
  if (mode === "static") return "skipped" as const;
});

// Inverse safety: a scenario using the morph-nav step without `@morph`
// would silently hang in static mode until the cucumber step ceiling.
Before(function (this: EmanoteWorld, scenario) {
  const usesMorph = scenario.pickle.steps.some((s) =>
    s.text.includes("navigate via Ema"),
  );
  const tagged = scenario.pickle.tags.some((t) => t.name === MORPH_TAG);
  if (usesMorph && !tagged) {
    throw new Error(
      `Scenario ${JSON.stringify(scenario.pickle.name)} uses 'navigate via Ema' but is not tagged ${MORPH_TAG}. Add '${MORPH_TAG}' above the Scenario keyword so it is skipped in static mode (no WebSocket; window.ema is undefined there).`,
    );
  }
});

// In morph mode the first `openRoute` morph-switches via
// `window.ema`, which only exists after a real page load — prime it.
Before(async function (this: EmanoteWorld) {
  if (mode !== "morph") return;
  await primeMorph(this.page);
});

After(async function (this: EmanoteWorld, scenario) {
  if (scenario.result?.status === Status.FAILED) {
    const dir = path.join(runRoot, "screenshots");
    fs.mkdirSync(dir, { recursive: true });
    const name = scenario.pickle.name.replace(/\s+/g, "-").toLowerCase();
    await this.page.screenshot({
      path: path.join(dir, `${name}-${mode}.png`),
      fullPage: true,
    });
  }
  if (this.context) await this.context.close();
});
