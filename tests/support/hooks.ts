/**
 * Cucumber hooks — browser lifecycle + emanote backend launcher.
 *
 * `EMANOTE_MODE` is the only axis that distinguishes runs:
 *   - `live`   → spawn `emanote -L <fixture> run --port N`
 *   - `static` → `emanote -L <fixture> gen <tmp>` once, then serve `<tmp>`
 *                on a random port with a zero-dep Node static server
 *
 * Step definitions only see `baseUrl` and never learn which mode they ran
 * in — any mode-specific branching in a step means the boundary has
 * leaked. Keep it encapsulated here.
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
import * as fs from "node:fs";
import * as http from "node:http";
import * as os from "node:os";
import * as path from "node:path";
import { spawn, type ChildProcess } from "node:child_process";
import { EmanoteWorld } from "./world.ts";

type Mode = "live" | "static";

const mode: Mode = (() => {
  const m = process.env.EMANOTE_MODE;
  if (m !== "live" && m !== "static") {
    throw new Error(
      `EMANOTE_MODE must be "live" or "static" (got ${JSON.stringify(m)})`,
    );
  }
  return m;
})();

const emanoteBin = (() => {
  const bin = process.env.EMANOTE_BIN;
  if (!bin) throw new Error("EMANOTE_BIN must point to the emanote binary");
  return bin;
})();

const fixtureDir = path.resolve(
  path.dirname(new URL(import.meta.url).pathname),
  "..",
  "fixtures",
  "notebook",
);

const runRoot = fs.mkdtempSync(path.join(os.tmpdir(), "emanote-e2e-"));

let browser: Browser;
let baseUrl: string;
let liveProc: ChildProcess | undefined;
let staticServer: http.Server | undefined;

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
    await new Promise((r) => setTimeout(r, 500));
  }
  throw new Error(
    `Backend did not serve ${url} containing ${JSON.stringify(needle)} within ${timeoutMs}ms (last error: ${String(lastErr)})`,
  );
}

async function startLive(): Promise<string> {
  const port = await getPort();
  liveProc = spawn(
    emanoteBin,
    ["-L", fixtureDir, "run", "--port", String(port), "--no-ws"],
    { stdio: ["ignore", "pipe", "pipe"] },
  );
  liveProc.stderr?.on("data", (d: Buffer) =>
    process.stderr.write(`[emanote:live] ${d}`),
  );
  liveProc.on("exit", (code, sig) => {
    if (code !== 0 && code !== null) {
      process.stderr.write(
        `[emanote:live] exited unexpectedly (code=${code} sig=${sig})\n`,
      );
    }
  });
  const url = `http://127.0.0.1:${port}`;
  await waitForHtml(url, "emanote-theme-remap", 60_000);
  return url;
}

async function startStatic(): Promise<string> {
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
  staticServer = http.createServer((req, res) => {
    const urlPath = decodeURIComponent((req.url ?? "/").split("?")[0]);
    const candidate =
      urlPath.endsWith("/") ? urlPath + "index.html" : urlPath;
    const filePath = path.join(outDir, candidate);
    if (!filePath.startsWith(outDir)) {
      res.statusCode = 403;
      res.end("forbidden");
      return;
    }
    fs.readFile(filePath, (err, data) => {
      if (err) {
        res.statusCode = 404;
        res.end("not found");
        return;
      }
      res.setHeader("Content-Type", contentTypeFor(filePath));
      res.end(data);
    });
  });
  await new Promise<void>((resolve) => staticServer!.listen(port, resolve));
  return `http://127.0.0.1:${port}`;
}

function contentTypeFor(p: string): string {
  const ext = path.extname(p).toLowerCase();
  switch (ext) {
    case ".html":
      return "text/html; charset=utf-8";
    case ".css":
      return "text/css; charset=utf-8";
    case ".js":
      return "text/javascript; charset=utf-8";
    case ".json":
      return "application/json; charset=utf-8";
    case ".svg":
      return "image/svg+xml";
    case ".png":
      return "image/png";
    case ".woff2":
      return "font/woff2";
    default:
      return "application/octet-stream";
  }
}

BeforeAll(async () => {
  baseUrl = mode === "live" ? await startLive() : await startStatic();
  browser = await chromium.launch({
    headless: process.env.HEADLESS !== "false",
    args: ["--no-sandbox", "--disable-setuid-sandbox", "--disable-gpu"],
  });
});

AfterAll(async () => {
  if (browser) await browser.close();
  if (liveProc) liveProc.kill("SIGTERM");
  if (staticServer)
    await new Promise<void>((r) =>
      staticServer!.close(() => r()),
    );
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
