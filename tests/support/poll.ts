/**
 * HTTP polling for live-mode assertions.
 *
 * Three @hot-reload steps share the same loop shape: request a URL,
 * inspect the response, sleep 200ms, retry until a deadline. Lifting
 * the loop into one helper keeps each step a 4-line wrapper over a
 * predicate + a failure-message builder.
 *
 * The page-DOM variant (`the article body contains … within N seconds`)
 * intentionally stays elsewhere — it uses Playwright's
 * `waitForFunction` and doesn't fit the request-then-inspect shape.
 */

import type { EmanoteWorld } from "./world.ts";

export interface PollAttempt {
  ok: boolean;
  status: number;
  body: string | null;
}

/** Poll @url until @done returns true or @timeoutMs elapses. On
 *  timeout, the helper throws the error returned by @onTimeout —
 *  callers shape the message there with access to the last attempt
 *  (status, ok flag, body prefix), so each step's failure text stays
 *  specific without duplicating the loop. */
export async function pollUrl(
  page: EmanoteWorld["page"],
  url: string,
  timeoutMs: number,
  done: (attempt: PollAttempt) => boolean,
  onTimeout: (last: PollAttempt) => Error,
): Promise<void> {
  const deadline = Date.now() + timeoutMs;
  let last: PollAttempt = { ok: false, status: 0, body: null };
  while (Date.now() < deadline) {
    const resp = await page.request.get(url);
    const ok = resp.ok();
    const body = ok ? await resp.text() : null;
    last = { ok, status: resp.status(), body };
    if (done(last)) return;
    await new Promise((r) => setTimeout(r, 200));
  }
  throw onTimeout(last);
}
