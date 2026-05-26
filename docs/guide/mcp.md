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

Emanote advertises the notebook under the `emanote://` scheme as one static export plus a per-note URI template:

| URI | MIME | What it returns |
|---|---|---|
| `emanote://export/metadata` | `application/json` | Metadata for every note — titles, source paths, parent routes, resolved links. Same shape as [`emanote export --format=metadata`](export.md). Use this to discover paths. |
| `emanote://note/{path}` | `text/markdown` | One note, by its source path (e.g. `emanote://note/guide/mcp.md`). Prefixed with a header block (`<!-- Source … -->`, `<!-- URL … -->`, `<!-- Title … -->`, `<!-- Wikilinks … -->`). |

`resources/list` returns only the metadata export. Emanote intentionally does **not** enumerate one entry per note: that scales linearly with notebook size and inflates context on every poll. `resources/templates/list` advertises the `emanote://note/{path}` template for clients that support [RFC 6570 URI templates](https://datatracker.ietf.org/doc/html/rfc6570); to address a specific note, construct a URI from the template and call `resources/read` directly. Discover the set of valid paths from `emanote://export/metadata` (every note's `srcPath`).

> [!note] No bundled-content export
> Earlier drafts of phase 2 exposed `emanote://export/content` (every note concatenated into a single Markdown blob). It was removed before merge: the same information is available via metadata + per-note reads, the blob blows context budgets on any non-trivial notebook (a 422-note notebook is well past any reasonable LLM window), and an MCP client that polls it re-reads the whole disk every time. The `emanote export --format=content` CLI still produces this artifact for human/script use; MCP is the wrong transport for batch export.

### Algorithmic complexity

Per-request cost, where _N_ = number of notes in the model and _R_ = total resolved relations (wikilinks + transclusions) across all notes:

| MCP method | Cost in notebook size |
|---|---|
| `initialize` | **O(1)** |
| `resources/list` | **O(1)** — one fixed static entry, independent of _N_ |
| `resources/templates/list` | **O(1)** — currently one template (per-note); grows with templated kinds, not with notebook size |
| `resources/read emanote://export/metadata` | **O(N + R)** — iterates every note and every relation; JSON-encodes the result |
| `resources/read emanote://note/{path}` | **O(log N + \|note\|)** — ixset lookup plus one file read |
| `tools/list`, `tools/call find_notes` | **O(N)** — linear scan over note titles and source paths |
| `tools/call get_backlinks` | **O(R)** — ixset lookup of relations pointing to the target |
| `tools/call resolve_wikilink` | **O(log N)** — ixset lookup by wikilink, plus ambiguity resolution against the `from` note |

Reads are uncached: every `resources/read` re-runs against the live model. There is no per-client throttling or coalescing — a client that loops over per-note reads will re-traverse the disk each time. Phase 4 (subscriptions) replaces polling with push notifications and removes the per-poll cost for clients that opt in.

### Per-client behaviour

- **Codex** sees the template in the model-side `list_mcp_resource_templates` tool and can call `read_mcp_resource` against any path. Works out of the box.
- **Claude Code**'s model-side read tool ([docs](https://code.claude.com/docs/en/mcp.md#use-mcp-resources)) reads any URI the model constructs, including ones derived from the template. The `@`-mention picker, however, fuzzy-searches only the enumerated `resources/list` entries — so users won't see individual notes there and must drive reads through the model (e.g. ask it to "read `guide/mcp.md`" or to call the `find_notes` tool — see [[#tools]]).
- **opencode** populates its attach picker from `resources/list` only; per-note attachment via UI is unavailable without an enumeration. Same model-driven workaround as Claude Code applies when the model itself drives reads.

## Tools

Emanote advertises three read-only query tools through MCP's `tools/list`. They share the live model snapshot used by resources and return JSON-encoded text payloads.

| Tool | Inputs | Returns |
|---|---|---|
| `find_notes` | `query` (substring), optional `limit` (1–100, default 20) | `{matches: [{path, title, uri}]}` — case-insensitive matches on title or source path |
| `get_backlinks` | `path` (e.g. `guide/mcp.md`) | `{backlinks: [{path, title, uri}]}` — notes that link to the given note |
| `resolve_wikilink` | `wikilink` (e.g. `guide/mcp`), optional `from` (source path for disambiguation) | `{result: "found"\|"missing"\|"ambiguous", …}` — resolves through the same path Emanote uses for inline `[[…]]` references |

Each `path` returned is also a valid suffix for the `emanote://note/{path}` template — chain `find_notes` (or `get_backlinks`) into `resources/read` to load the full note. `resolve_wikilink` is the structured equivalent of asking Emanote what `[[…]]` would resolve to from the current note, including ambiguity disambiguation by closest common ancestor.

Errors surface in two ways: malformed input that can't be parsed (unknown path, empty wikilink) comes back as a text result with `isError: true`; a missing required argument comes back as JSON-RPC error `-32602` per the MCP protocol.

## Debugging

- Pass `-v` / `--verbose` to Emanote and the underlying `mcp` library will print one `[request]` / `[response]` line per JSON-RPC call to stdout. Useful when a client is misbehaving or you want to see exactly what a tool call looks like.
- If MCP fails to bind (port already in use, privileged port without capability), Emanote exits with the socket exception — MCP and the live server share process lifetime, so neither runs when the other can't start.
- MCP is not enabled unless `--mcp-port` is present under the `run` subcommand. `emanote gen` never starts MCP.

## Authentication

There is none today. The server is intended for local use, bound to a loopback port. Do not expose the MCP port to the public internet or to an untrusted network.
