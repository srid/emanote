---
slug: mcp
---

# MCP server

> [!warning] Work in progress
> MCP support is rolling out in phases ([#645](https://github.com/srid/emanote/issues/645)). **Read-only resources** are live as of this release — query tools and subscriptions arrive in later PRs. Expect the tool/prompt surface to grow until this notice is removed.

Emanote can expose an [MCP (Model Context Protocol)](https://modelcontextprotocol.io) endpoint beside its [[live-server|live server]], so that [Claude Code](https://claude.com/claude-code), [Codex](https://github.com/openai/codex), or any other MCP-aware client can query your notebook directly from the same process that renders it.

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

Start Emanote in one terminal (`emanote run --mcp-port 8079`), launch Claude Code in the same directory, and it will connect on startup. Use `/mcp` inside Claude Code to verify the server appears and list its tools/resources.

## Resources

Emanote advertises the notebook as three URI schemes under the `emanote://` scheme:

| URI | MIME | What it returns |
|---|---|---|
| `emanote://export/metadata` | `application/json` | Metadata for every note — titles, source paths, parent routes, resolved links. Same shape as [`emanote export --format=metadata`](export.md). |
| `emanote://export/content` | `text/markdown` | All notes concatenated into a single Markdown document with delimiters and an LLM-oriented preamble. Same shape as [`emanote export --format=content`](export.md). |
| `emanote://note/{path}` | `text/markdown` | One note, by its source path (e.g. `emanote://note/guide/mcp.md`). Prefixed with a header block (`<!-- Source … -->`, `<!-- URL … -->`, `<!-- Title … -->`, `<!-- Wikilinks … -->`). |

`resources/list` returns the two static exports plus one entry per note; `resources/templates/list` advertises the `emanote://note/{path}` template for clients that support [RFC 6570 URI templates](https://datatracker.ietf.org/doc/html/rfc6570).

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

## Debugging

- Pass `-v` / `--verbose` to Emanote and the underlying `mcp` library will print one `[request]` / `[response]` line per JSON-RPC call to stdout. Useful when a client is misbehaving or you want to see exactly what a tool call looks like.
- If MCP fails to bind (port already in use, privileged port without capability), Emanote exits with the socket exception — MCP and the live server share process lifetime, so neither runs when the other can't start.
- MCP is not enabled unless `--mcp-port` is present under the `run` subcommand. `emanote gen` never starts MCP.

## Authentication

There is none today. The server is intended for local use, bound to a loopback port. Do not expose the MCP port to the public internet or to an untrusted network.
