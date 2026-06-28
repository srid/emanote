---
name: odu-mcp
description: odu MCP server launcher — drive CI from a coding agent. `bin/serve` resolves odu via Nix and runs `odu mcp` in the cwd. See the repo README for the tools/resources and override knobs.
user-invocable: false
---

# odu-mcp

The agent face of [odu](https://github.com/juspay/odu) — an MCP stdio server
that re-exposes a live CI run as agent tools (`run`, `node_rerun`,
`wait_for_settle`, `cancel`) and subscribable resources (`surface://streams/nodes`,
`surface://collections/logs/{id}`), so Claude Code / Codex / opencode / Gemini
CLI drive CI with structured calls instead of scraping terminal output.

`cancel` stops the live run and waits until it's torn down; `run`'s `supersede`
cancels a run already live here before starting (the "stop this, run the fixed
commit" move), and `linger` keeps the coordinator serving past settle so a node
can be rerun afterwards. Together they let the agent loop call off or replace a
run instead of stranding it or hitting "a run is already in progress".

`bin/serve` is self-contained — it resolves odu via `nix run` and serves over
stdio in the consumer's repo (dialing `.ci/odu.sock`). Set `ODU_FLAKE` to
override the odu flake-ref (default `github:juspay/odu`); a repo that
re-exports odu can point it at its own pinned output with `ODU_FLAKE=.#odu`.

Full docs in the [repo README](https://github.com/juspay/odu/blob/master/README.md).

This skill primitive exists for APM's deployment convention — it lands
`bin/serve` at `.agents/skills/odu-mcp/bin/serve` in the consumer's working
tree (APM's skills-convergence path), which keeps the launcher available even
before `apm install` runs on a fresh clone. The package is mechanically a
"skill" in APM's primitive vocabulary; semantically it's a tool launcher.
