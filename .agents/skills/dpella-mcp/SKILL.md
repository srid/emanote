---
name: dpella-mcp
description: Reference for the dpella/mcp Haskell library — types, handler shape, transports, and the conventions Emanote uses. Load this before adding MCP resources/tools/subscriptions so you don't re-discover the API by WebFetch.
user-invocable: false
---

# dpella/mcp reference

Canonical source: <https://github.com/dpella/mcp>. Two Hackage packages: `mcp-types` (pure types, minimal deps) and `mcp` (Servant-based server with HTTP/Stdio transports). MCP protocol version `2025-06-18`. Depends on `jsonrpc` (also a DPella package, not in nixpkgs).

## Versions to pin

Use GitHub sources rather than Hackage when the target GHC's `all-cabal-hashes` snapshot lags behind:

```nix
# flake.nix inputs
dpella-mcp.url = "github:dpella/mcp";
dpella-mcp.flake = false;
dpella-jsonrpc.url = "github:dpella/jsonrpc";
dpella-jsonrpc.flake = false;
```

```nix
# haskell-flake
packages.mcp.source       = inputs.dpella-mcp + /mcp-server;
packages.mcp-types.source = inputs.dpella-mcp + /mcp-types;
packages.jsonrpc.source   = inputs.dpella-jsonrpc;
```

Supported GHC: 9.6–9.12 (via `base >=4.18 && <4.22`). `servant-auth-server` 0.4.x and `warp` 3.4.x are sibling deps.

## Required type-family instances

Every user of `MCP.Server` must provide both:

```haskell
type instance MCPHandlerState = YourSessionState
type instance MCPHandlerUser  = YourUserPayload  -- for JWT; use () when using simpleHttpApp
```

Instances must live in a module that gets compiled before `initMCPServerState` is called.

## Three transports

| Transport | Auth | Entry point | Notes |
|---|---|---|---|
| HTTP + JWT | servant-auth-server | `mcpAPI stateVar` via `serveWithContext (Proxy @MCPAPI) ctx` | Production web |
| Simple HTTP | none | `simpleHttpApp stateVar :: Application` | Local/loopback only; pair with `Warp.run` or `Warp.runSettings` |
| Stdio | none | `serveStdio stdin stdout initialState` | Subprocess integrations |

For HTTP use `Warp.setBeforeMainLoop` to log after the socket is bound — do not print before `Warp.run`/`runSettings`, the bind hasn't happened yet and the log lies.

## `initMCPServerState` signature

```haskell
initMCPServerState
  :: MCPHandlerState                                                  -- initial state
  -> Maybe (MCPHandlerUser -> MCPHandlerState -> IO MCPHandlerState)  -- onInitialize (JWT only)
  -> Maybe (MCPHandlerState -> IO MCPHandlerState)                    -- onFinalize (after each req)
  -> ServerCapabilities                                               -- what to advertise
  -> Implementation                                                   -- name/version/title
  -> Maybe Text                                                       -- free-text instructions
  -> ProcessHandlers                                                  -- the actual handlers
  -> MCPServerState
```

Wrap the result in an `MVar` — `simpleHttpApp` / `mcpAPI` both take `MVar MCPServerState`.

## `ServerCapabilities` record

```haskell
ServerCapabilities
  { logging      :: Maybe LoggingCapability
  , prompts      :: Maybe PromptsCapability    { listChanged :: Maybe Bool }
  , resources    :: Maybe ResourcesCapability  { listChanged, subscribe :: Maybe Bool }
  , tools        :: Maybe ToolsCapability      { listChanged :: Maybe Bool }
  , completions  :: Maybe CompletionsCapability
  , experimental :: Maybe ...
  }
```

Advertising `Just …` without providing a matching handler means clients will call it and hit the library's `method_not_found`. For Phase-1-style stubs, provide an empty-list handler (e.g. `listResourcesHandler = Just (\_ -> pure $ ProcessSuccess ListResourcesResult{..})`).

## `ProcessHandlers` record

Every field is a `Maybe` — set only what the server implements:

- `listResourcesHandler`, `readResourceHandler`, `listResourceTemplatesHandler`
- `listToolsHandler`, `callToolHandler` — **don't set manually**; use `withToolHandlers :: [ToolHandler] -> ProcessHandlers -> ProcessHandlers` which wires both based on a list of `ToolHandler`s.
- `listPromptsHandler`, `getPromptHandler`
- `completeHandler`
- `subscribeHandler`, `unsubscribeHandler`

Start from `defaultProcessHandlers` (all `Nothing`) and override.

## `ToolHandler` construction

```haskell
toolHandler
  :: Text                         -- name
  -> Maybe Text                   -- description
  -> InputSchema                  -- JSON schema for args
  -> (Maybe (Map Text Value)      -- handler body
        -> MCPServerT (ProcessResult CallToolResult))
  -> ToolHandler
```

`InputSchema "object" (Just propsMap) (Just ["req1", "req2"])` is the typical shape. `withToolHandlers` validates required args before calling the handler.

Returning results: `toolTextResult [Text]` for plain text; build `CallToolResult` directly when you need structured output (`structuredContent :: Maybe (Map Text Value)`).

## Logging

`mcp_log_level :: Maybe LoggingLevel` on `MCPServerState` — defaults to `Just Warning`. Set to `Just Debug` to get one `[request]` / `[response]` stdout line per JSON-RPC call (see `MCP/Server/HTTP/Internal.hs`). The library does **not** emit anything else on its own; wire your own startup log via Warp's `setBeforeMainLoop`.

Clients can also change the level at runtime via `logging/setLevel` if the server advertises `logging = Just LoggingCapability`.

## `ProcessResult` shape

```haskell
data ProcessResult a
  = ProcessSuccess a
  | ProcessRPCError Int Text             -- JSON-RPC error (400-ish, 404-ish, …)
  | ProcessServerError Text              -- internal
  | ProcessClientInput Text Value cont   -- request additional input from client (sampling, elicitation)
```

Return `ProcessRPCError 404 ("resource not found: " <> uri)` for unknown URIs; the library turns this into a well-formed JSON-RPC error.

## `DuplicateRecordFields` is required

MCP types reuse `name`, `_meta`, `title`, etc. across many records (`Implementation`, `Tool`, `Prompt`, `Resource`, …). Enable `{-# LANGUAGE DuplicateRecordFields #-}` at the use site. Field disambiguation sometimes needs a qualified prefix: `MCP.name = …`, `MCP._meta = …` where `import MCP.Server qualified as MCP`.

## Emanote-specific conventions (phase 1)

- MCP runs beside the live server via `UnliftIO.Async.race_` under the `run` subcommand only.
- CLI flag: `emanote run --mcp-port PORT` (top-level `--verbose` flips library log level to `Debug`).
- Startup line: `[mcp] listening on http://localhost:PORT/mcp` via `Warp.setBeforeMainLoop`.
- Transport: `simpleHttpApp` (no auth). Authentication deferred per the issue's open question.
- State types: `MCPHandlerState = ()`, `MCPHandlerUser = ()`.
- Handlers advertise `resources` and `tools` capabilities; empty inventories today.
- See `emanote/src/Emanote/MCP.hs` for the canonical shape.

Rollout tracker: [srid/emanote#645](https://github.com/srid/emanote/issues/645). Phases 2–5 will add notebook-backed resources, query tools, subscriptions, and optional prompts respectively.

## Common pitfalls

1. **Version tax**: Hackage's `mcp` moves faster than nixpkgs' `all-cabal-hashes`. Pin GitHub sources in `flake.nix` inputs when the Hackage-via-nixpkgs path refuses to build.
2. **`newMVar` ambiguity**: importing `Control.Concurrent.MVar` alongside Relude produces an ambiguous `newMVar`. Drop the explicit import — Relude's is fine.
3. **`NamedFieldPuns` with qualified imports**: `ReadResourceParams {uri}` only works if `uri` is unqualified. If you import `MCP.Server` qualified, either enable `NamedFieldPuns` and use the unqualified name, or bind fields positionally.
4. **Ambiguous `_meta`**: field is shared across a dozen records; with `DuplicateRecordFields` on, GHC often still demands a qualified prefix (`MCP._meta`) at construction sites. `fourmolu` will re-add the qualifier if removed.
5. **Don't advertise `subscribe = Just True`** until the server actually implements `subscribeHandler` / `unsubscribeHandler`. The library has no built-in defaults for those.
