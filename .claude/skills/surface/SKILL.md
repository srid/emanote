---
name: surface
description: >-
  How a downstream app consumes the shared @kolu/surface stack (@kolu/surface ·
  surface-app · surface-nix-host · surface-mcp) — declaring a typed reactive surface,
  serving it, consuming it (SolidJS hooks or a CLI), and mirroring a remote surface over
  ssh. Grounded in the real consumers: kolu, pulam-web, drishti, odu, and the TUIs. Load
  when wiring a surface server/client/mirror, or reaching for getHostSession / a link /
  the `.use()` hooks. CHANGING the framework is gated separately —
  `.claude/rules/surface.md` (a paired, CI-green drishti PR pinned to final kolu HEAD).
---

# Using @kolu/surface (downstream consumer guide)

Declare a typed reactive surface once; the framework derives the oRPC contract, wires the
server, and binds the Solid client hooks. **This is the consumer guide** — *changing* the
framework needs a paired drishti PR (`.claude/rules/surface.md`).

## Who uses it — match the closest consumer, don't hand-roll

| Consumer | Shape | Notable |
| --- | --- | --- |
| **kolu** (`client`+`server`) | one browser ⇄ one Node server, ONE ws | single-tier; two sibling surfaces; uses `surface`+`surface-app` only (**not** `-nix-host`) |
| **pulam-web** | browser ⇄ Node ⇄ ssh fleet mirror | one ws per host (`/rpc/ws?host=`); `connectSurface`; re-serves `terminalWorkspaceSurface` |
| **drishti** (`srid/drishti`) | browser ⇄ Node ⇄ ssh agent mirror | the canonical twin; 3 workspaces (common/agent/app) |
| **odu** (`juspay/odu`) | CI runner: stdio lanes → unix-socket fan-in → CLI/MCP | serve+consume+mirror over every transport at once; `surface-mcp` projection |
| **pulam-tui / kaval-tui** | one-shot CLI/TUI, no browser | transport-blind `{client,dispose}`; unix-socket local, ssh remote; **no `.use()` hooks** |

## The spine (real import paths)

- **Define** — `defineSurface({cells,collections,streams,events,procedures})` (`@kolu/surface/define`). Many surfaces over one transport: `composeSurfaceContracts(map)` + sibling clients, never merged.
- **Serve** — `implementSurface(surface, deps)` / `implementSurfaces(map, fwDeps, perKeyDeps)` (`@kolu/surface/server`; `inMemoryStore` / `inMemoryChannelByName` back the cells/channels). **Always flatten before serving:** `implement(surface.contract).router({ ...fragment.router })` — else oRPC double-prefixes `/surface/surface/…` and every call 404s.
- **Consume (SolidJS)** — `surfaceClient(surface, link)` / `surfaceClients(link, map)` (`@kolu/surface/solid`) → `client.cells.X.use({authority,initial,onError})`, `.collections.X.use({keys,onError})` then `.byKey(id)?.()` / `.keys()`, `.streams.X.use(inputFn,{onError})` with `.pending()`/`.error()`, `.events.X.use(inputFn,handler)`.
- **Consume (CLI/TUI)** — no reactive hooks; raw awaited `conn.client.surface.<verb>(…)` + async-iterator iteration; a live board uses `mirrorRemoteSurface(spec, client, {collections,streams}, {log})` (`@kolu/surface/mirror`) into plain callbacks.

## Links (transport, swappable)

`websocketLink(ws)` (`/links/websocket`) · `stdioLink` (`/links/stdio`) · `unixSocketLink({socketPath})` (`/links/unix-socket`) · `directLink(router)` (`/links/direct`, in-process identity, for tests). Serve side: `serveOverStdio` (`/peer-server`), `serveOverUnixSocket` (`/unix-socket`), oRPC `RPCHandler` (`@orpc/server/ws`, `.upgrade(ws)`) for browsers. CLIs keep ONE transport-blind `Connection = {client, dispose}` so every command is written once across local vs ssh.

## Mirror a remote surface (drishti / pulam-web / odu)

1. **Dial the host** — `getHostSession<contract>({host, binary, resolveDrvPath})` (`@kolu/surface-nix-host`): long-lived, `nix copy`s the agent closure, runs `<bin> --stdio` over ssh, reconnects. `buildHostRegistry` fans out N hosts; one-shot CLIs use `dialAgentOnce` instead.
2. **Mirror inward** — `pumpRemoteSurface({source, session, makeSink, …})` (`-nix-host`) folds the remote agent's frames into a local `implementSurface` re-serve via a `SurfaceSink` (`makeSink`, `@kolu/surface/mirror`). The parent implements the *same* surface; a remotely-unobservable cell (e.g. connection state) is parent-authoritative.
3. **Re-serve** — the local fragment served on `/rpc/ws`, accepted via `acceptSurfaceSocket` (`@kolu/surface-app/server`). Browsers connect with `connectSurface` (`@kolu/surface-app/solid`), which bundles socket + `websocketLink` + `surfaceClient` + a default-on liveness heartbeat.

## Gotchas (hard-won, all real)

- **Procedures call off the FULL link, not the scoped client** — `surfaceClients` per-key `.rpc` is typed `unknown`; reach the root link for raw procedures.
- **Raw streaming** — `unenrolledStreamCall(client.X, input, {signal, onRetry})` (`@kolu/surface/client`) carries the reconnect (`STREAM_RETRY`) context; a bare `client.X(…)` silently loses it. There is **no `stream` namespace** (`.claude/rules/streaming.md` is stale on that point).
- **Consume streams fine-grained** — value-bearing → `.streams.use()` (replace-each-frame); delta-accumulate → `mirrorRemoteSurface` / `createSubscription`+`reduce`. Never coarse-read-and-copy: same-shape frames coalesce and the view freezes.
- **Snapshot-then-deltas + fail-fast** — a cell always opens with a snapshot; `firstFrameOrThrow` (`@kolu/surface/first-frame`) treats an empty stream as a link failure, never a silent empty.
- **Liveness is on by construction** — framework-reserved `surface.system.live`; `connectSurface` / `HostSession` / `createServerLifecycle` default their watchdog to it (`probeSurfaceLive`). Don't nominate your own probe unless you mean to (pulam-tui's version-cell probe is the rare, deliberate exception).
- **Version skew** — gate on `isContractVersionCompatible` (major.minor), never a string `==`.
- **Nix-baked deps** — odu declares no `@kolu/*` in `package.json`; they're symlinked at Nix build (the bake-in-via-Nix convention). A bare `pnpm install` won't resolve them.

## Reference

Runnable end-to-end: `packages/surface/example/` (its `mini-ci` stdio example is odu's seed). Full API + rationale: each package's `README.md`. Match the closest consumer above; don't reinvent a primitive the table already shows how to use.
