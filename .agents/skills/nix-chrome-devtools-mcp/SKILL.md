---
name: nix-chrome-devtools-mcp
description: chrome-devtools-mcp launcher — a single bash script that resolves Playwright's bundled Chrome-for-Testing and Node.js directly through Nix. See repo README for usage, override mechanisms, and version compatibility.
user-invocable: false
---

# nix-chrome-devtools-mcp

Drop-in launcher for the [Chrome DevTools MCP server](https://github.com/ChromeDevTools/chrome-devtools-mcp). Self-contained — `bin/serve` resolves all inputs via `nix build` / `nix shell`.

Full docs in the [repo README](https://github.com/juspay/nix-chrome-devtools-mcp/blob/main/README.md).

This skill primitive exists for APM's deployment convention — it ensures `bin/serve` lands at `.agents/skills/nix-chrome-devtools-mcp/bin/serve` in the consumer's working tree (per APM's [skills convergence](https://microsoft.github.io/apm/reference/targets-matrix/#skills-convergence) path), which keeps the launcher available even before `apm install` runs on a fresh clone. The package is mechanically a "skill" in APM's primitive vocabulary; semantically it's a tool.
