---
name: nix-justfile
description: Conventions for writing justfile recipes in Nix-based projects.
user-invocable: false
---

# Justfile in Nix projects

## Doc comments

Every recipe must have a doc comment (`#` line above the recipe name).

## Devshell detection

Use a `nix_shell` variable to auto-detect whether we're inside `nix develop`:

```just
nix_shell := if env('IN_NIX_SHELL', '') != '' { '' } else { 'nix develop -c' }
```

Prefix commands that need devshell tools (node, trunk, cargo-watch, etc.) with `{{nix_shell}}`. Recipes that invoke `nix` directly (build, run, flake commands) don't need it.

## Legibility

Long chained commands should use multiline `\` continuations.
