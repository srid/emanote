---
name: nix-playwright
description: Use this when adding Nix-based local runs for an existing Playwright e2e suite. Provides a self-contained `tests/shell.nix` that uses `nixpkgs-latest` for `playwright-driver.browsers`, plus a justfile entry — works on NixOS where `npx playwright install --with-deps` cannot.
---

# Playwright e2e under Nix

For projects that already have a Playwright e2e suite (typically under `tests/`) and need it to run locally on NixOS / pure-Nix hosts, where Playwright's bundled Chromium download depends on `apt-get` + `sudo`.

GitHub Actions CI is left untouched — apt-provided Chromium on Ubuntu runners is cheaper than spinning up a Nix devshell. This skill is for **local** runs (and Nix-based CI like Vira); GHA continues to use its own apt path.

## Add a `nixpkgs-latest` flake input

In top-level `flake.nix`, add an independent pin so the Playwright rev moves on its own cadence (decoupled from the main `nixpkgs` your build uses):

```nix
inputs.nixpkgs-latest.url = "github:nixos/nixpkgs/nixpkgs-unstable";
```

## `tests/shell.nix`

Self-contained. Reads the parent flake's lock to resolve `nixpkgs-latest` without needing a flake context:

```nix
let
  lock = builtins.fromJSON (builtins.readFile ../flake.lock);
  nixpkgsLocked = lock.nodes.nixpkgs-latest.locked;
  pkgs = import (builtins.fetchTree {
    type = "github";
    inherit (nixpkgsLocked) owner repo rev narHash;
  }) { };
in
pkgs.mkShell {
  packages = [ pkgs.nodejs ];
  PLAYWRIGHT_BROWSERS_PATH = "${pkgs.playwright-driver.browsers}";
  PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD = "1";
}
```

## Justfile recipe

```just
e2e:
    nix-shell tests/shell.nix --run "cd tests && { [ -d node_modules ] || npm ci; } && npm test"
```

## Critical version constraint

`pkgs.playwright-driver` version must match the `playwright` version in `tests/package.json`. Drift raises **"browser revision X not found at <PLAYWRIGHT_BROWSERS_PATH>"** at launch.

Bump both together:

1. `nix flake update nixpkgs-latest`
2. Match `playwright` in `tests/package.json` to the new driver version
3. `npm install` in `tests/` to regenerate the lockfile

## Reference

Pattern extracted from [emanote#660](https://github.com/srid/emanote/pull/660).
