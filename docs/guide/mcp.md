---
slug: mcp
---

# MCP server

> [!warning] Work in progress
> MCP support is rolling out in phases ([#645](https://github.com/srid/emanote/issues/645)). The current release ships only the HTTP transport and the lifecycle handshake — resources, tools, and subscriptions arrive in later PRs. Expect the surface to grow and the wire details to shift until this notice is removed.

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
