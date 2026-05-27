---
slug: mcp
---

# MCP server

> [!warning] Work in progress
> MCP support is rolling out in phases ([#645](https://github.com/srid/emanote/issues/645)). **Read-only resources and query tools** are live as of this release; subscriptions and prompts arrive in later PRs. Expect the tool/prompt surface to grow until this notice is removed.

Emanote can expose an [MCP (Model Context Protocol)](https://modelcontextprotocol.io) endpoint beside its [live server](https://ema.srid.ca/topics/live-server), so that [Claude Code](https://claude.com/claude-code), [Codex](https://github.com/openai/codex), or any other MCP-aware client can query your notebook directly from the same process that renders it.

Enable it by passing `--mcp-port PORT` to `emanote run`:

```sh
emanote run --port 9010 --mcp-port 8079
```

Emanote prints one line to stderr once the MCP endpoint is ready:

```
[mcp] listening on http://localhost:8079/mcp
```

## Client setup

### Claude Code

Claude Code reads MCP server configuration from `.mcp.json` in your project root (or your home directory). Point it at the running Emanote:

```json
{
  "mcpServers": {
    "emanote": {
      "url": "http://localhost:8079/mcp"
    }
  }
}
```

Start Emanote in one terminal (`emanote run --mcp-port 8079`), launch Claude Code in the same directory, and it will connect on startup. Use `/mcp` inside Claude Code to verify the server appears and list its resources.

### Codex

Codex uses [`~/.codex/config.toml`](https://github.com/openai/codex#mcp-servers) for MCP servers. HTTP transport wiring looks like:

```toml
[mcp_servers.emanote]
url = "http://localhost:8079/mcp"
```

Restart Codex after editing the config; it will pick up the server on next launch.

### Quick sanity check with `curl`

MCP is JSON-RPC over HTTP with SSE responses. A raw `initialize` call:

```sh
curl -sS -N -X POST http://localhost:8079/mcp \
  -H 'Content-Type: application/json' \
  -H 'Accept: application/json, text/event-stream' \
  -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"curl","version":"1.0"}}}'
```

You should see an SSE `event: message` frame carrying the server's implementation metadata and advertised capabilities.

## Resources

Emanote advertises the notebook under the `emanote://` scheme as a single static export:

| URI | MIME | What it returns |
|---|---|---|
| `emanote://export/metadata` | `application/json` | Metadata for every note — titles, source paths, parent routes, resolved links. Same shape as [`emanote export --format=metadata`](../authoring/export.md). Use this to discover paths. |

`resources/list` returns only the metadata export. Emanote intentionally does **not** enumerate one entry per note: that scales linearly with notebook size and inflates context on every poll. Clients discover note paths from `emanote://export/metadata` (every note's `filePath`) and the query tools below; the underlying files are read through the client's own filesystem tools (e.g. Claude Code's `Read`, Codex's local file access), not through MCP.

> [!note] Why per-note bodies aren't served over MCP
> MCP clients already have direct filesystem access — a separate `resources/read emanote://note/{path}` handler would just duplicate that, and a bundled-content export (every note concatenated into one Markdown blob) blows context budgets on any non-trivial notebook (a 422-note notebook is well past any reasonable LLM window) and forces a full disk re-read on every poll. The `emanote export --format=content` CLI still produces the bundled artifact for human/script use; MCP is the wrong transport for batch export. Earlier phases of this server exposed both surfaces; both were removed.

### Algorithmic complexity

Per-request cost, where _N_ = number of notes in the model and _R_ = total resolved relations (wikilinks + transclusions) across all notes:

| MCP method | Cost in notebook size |
|---|---|
| `initialize` | **O(1)** |
| `resources/list` | **O(1)** — one fixed static entry, independent of _N_ |
| `resources/templates/list` | **O(1)** — currently empty |
| `resources/read emanote://export/metadata` | **O(N + R)** — iterates every note and every relation; JSON-encodes the result |
| `tools/list`, `tools/call find_notes` | **O(N)** — linear scan over note titles and source paths |
| `tools/call get_backlinks` | **O(R)** — ixset lookup of relations pointing to the target |
| `tools/call resolve_wikilink` | **O(log N)** — ixset lookup by wikilink, plus ambiguity resolution against the `from` note |

Reads are uncached: every `resources/read` re-runs against the live model. Phase 4 (subscriptions) replaces polling with push notifications and removes the per-poll cost for clients that opt in.

## Tools

Emanote advertises three read-only query tools through MCP's `tools/list`. They share the live model snapshot used by resources and return JSON-encoded text payloads.

| Tool | Inputs | Returns |
|---|---|---|
| `find_notes` | `query` (substring), optional `limit` (1–100, default 20) | `{matches: [{path, title}]}` — case-insensitive matches on title or source path |
| `get_backlinks` | `path` (e.g. `guide/mcp.md`) | `{backlinks: [{path, title}]}` — notes that link to the given note |
| `resolve_wikilink` | `wikilink` (e.g. `guide/mcp`), optional `from` (source path for disambiguation) | `{result: "found"\|"missing"\|"ambiguous", …}` — resolves through the same path Emanote uses for inline `[[…]]` references |

Each `path` returned is a notebook-relative source path (e.g. `guide/mcp.md`) — feed it to the client's own filesystem read tool to load the body. `resolve_wikilink` is the structured equivalent of asking Emanote what `[[…]]` would resolve to from the current note, including ambiguity disambiguation by closest common ancestor.

Errors surface in two ways: malformed input that can't be parsed (unknown path, empty wikilink) comes back as a text result with `isError: true`; a missing required argument comes back as JSON-RPC error `-32602` per the MCP protocol.

## Debugging

- Pass `-v` / `--verbose` to Emanote and the underlying `mcp` library will print one `[request]` / `[response]` line per JSON-RPC call to stdout. Useful when a client is misbehaving or you want to see exactly what a tool call looks like.
- If MCP fails to bind (port already in use, privileged port without capability), Emanote exits with the socket exception — MCP and the live server share process lifetime, so neither runs when the other can't start.
- MCP is not enabled unless `--mcp-port` is present under the `run` subcommand. `emanote gen` never starts MCP.

## Authentication

There is none today. The server is intended for local use, bound to a loopback port. Do not expose the MCP port to the public internet or to an untrusted network.
